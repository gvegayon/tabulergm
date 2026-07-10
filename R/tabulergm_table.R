#' Generate a Publication-Ready Table from an ERGM Object
#'
#' S3 generic that dispatches to methods for fitted [ergm][ergm::ergm] objects
#' or plain [formula][stats::formula] objects, calling the internal parsing
#' engine and returning a formatted table.
#'
#' @param object A fitted [ergm][ergm::ergm] object or an ERGM
#'   [formula][stats::formula].
#' @param ... Additional arguments passed to methods.
#' @return A `data.frame` (default), or a `knitr_kable` object when
#'   `format` is `"html"` or `"markdown"`. When the term figures use
#'   drawing conventions (orange for focal attributes, orange/teal for
#'   mixing, squares/circles for bipartite modes), an explanatory note is
#'   appended below `"html"` and `"markdown"` tables.
#' @export
#' @seealso [tabulergm_table.ergm()], [tabulergm_table.formula()]
tabulergm_table <- function(object, ...) {
  UseMethod("tabulergm_table")
}

#' @describeIn tabulergm_table Method for fitted ergm objects.
#'
#' Calls [parse_ergm_model()] and returns a table with default columns
#' `term`, `figure`, `estimate`, `se`, and `pvalue`.
#' Optional columns (`description`, `math`, `attribute`) can be included
#' via logical arguments.
#'
#' @param include_description Logical. Include the term description column?
#'   Default `FALSE`.
#' @param include_math Logical. Include the mathematical notation column?
#'   Default `FALSE`.
#' @param include_attribute Logical. Include the attribute column? Default
#'   `FALSE`.
#' @param format Character. Output format: `"data.frame"` (default),
#'   `"html"`, or `"markdown"`. HTML and Markdown output require the
#'   \pkg{knitr} package.
#' @param figures_dir Optional directory for figure assets when
#'   `format = "markdown"`. When `NULL`, figures are copied automatically to
#'   the active knitr/Quarto/R Markdown figure path during non-interactive
#'   document rendering.
#'
#' @export
#' @examples
#' library(ergm)
#' fit <- readRDS(system.file("fits", "fit_edges.rds", package = "tabulergm"))
#' tabulergm_table(fit)
#' tabulergm_table(fit, include_description = TRUE)
#' tabulergm_table(fit, format = "markdown")
tabulergm_table.ergm <- function(
    object,
    include_description = FALSE,
    include_math = FALSE,
    include_attribute = FALSE,
    format = c("data.frame", "html", "markdown"),
    figures_dir = NULL,
    ...) {

  format <- match.arg(format)

  parsed <- parse_ergm_model(object)

  # Default columns
  cols <- c("term", "figure", "estimate", "se", "pvalue")

  # Optional columns
  if (include_description) cols <- c(cols, "description")
  if (include_math)        cols <- c(cols, "math")
  if (include_attribute)   cols <- c(cols, "attribute")

  result <- parsed[, cols, drop = FALSE]
  rownames(result) <- NULL

  .format_output(result, format, figures_dir = figures_dir)
}

#' @describeIn tabulergm_table Method for formula objects.
#'
#' Calls [parse_ergm_formula()] and returns a table with columns
#' `term`, `figure`, `math`, and `description`. Coefficient statistics
#' are excluded because no fitted model is available.
#'
#' @export
#' @examples
#' library(ergm)
#' tabulergm_table(network ~ edges + nodematch("gender"))
tabulergm_table.formula <- function(
    object,
    format = c("data.frame", "html", "markdown"),
    figures_dir = NULL,
    ...) {

  format <- match.arg(format)

  parsed <- parse_ergm_formula(object)

  # Formula-only columns (no coefficient statistics)
  cols <- c("term", "figure", "math", "description")

  result <- parsed[, cols, drop = FALSE]
  rownames(result) <- NULL

  .format_output(result, format, figures_dir = figures_dir)
}


# ---- Internal Helpers: Output Formatting ----

#' Pre-process math and figure columns for formatted output
#'
#' For `"markdown"` and `"html"` formats:
#' * `math` values are wrapped in output-specific delimiters: `$$...$$` for
#'   HTML output and `$...$` for Markdown output.
#' * `figure` file-paths are converted to output-specific image markup:
#'   Markdown image syntax for Markdown output and `<img>` tags for HTML.
#'
#' @param df A data frame containing the table.
#' @param format One of `"data.frame"`, `"html"`, or `"markdown"`.
#' @param figures_dir Optional figure asset directory for Markdown output.
#' @param copy_figures Logical. Copy Markdown figures to `figures_dir` or the
#'   active knitr figure path?
#' @return A modified copy of `df`.
#' @noRd
.preprocess_columns <- function(
    df,
    format,
    figures_dir = NULL,
    copy_figures = TRUE) {
  if (format == "data.frame") return(df)

  figures_dir <- .validate_figures_dir(figures_dir)

  # Math column: wrap non-NA values in display-math delimiters
  if ("math" %in% names(df)) {
    has_math <- !is.na(df[["math"]]) & nzchar(trimws(df[["math"]]))
    math_values <- trimws(df[["math"]][has_math])
    if (format == "html") {
      math_values <- vapply(
        math_values,
        .escape_math_html,
        FUN.VALUE = character(1)
      )
      df[["math"]][has_math] <- paste0("$$", math_values, "$$")
    } else {
      df[["math"]][has_math] <- paste0("$", math_values, "$")
    }
  }

  # Figure column: convert file paths to <img> tags
  if ("figure" %in% names(df)) {
    if (format == "markdown" && isTRUE(copy_figures)) {
      df <- .copy_markdown_figures(df, figures_dir = figures_dir)
    }

    has_fig <- !is.na(df[["figure"]]) & nzchar(df[["figure"]])
    fig_src <- vapply(
      df[["figure"]][has_fig],
      .figure_src_for_format,
      FUN.VALUE = character(1),
      format = format
    )
    if (format == "markdown") {
      df[["figure"]][has_fig] <- sprintf(
        '![](%s){width=80px}',
        fig_src
      )
    } else {
      df[["figure"]][has_fig] <- sprintf(
        '<img src="%s" style="width:80px;max-width:100%%;" alt="term figure">',
        fig_src
      )
    }
    df[["figure"]][!has_fig] <- ""
  }

  df
}


#' Escape HTML-sensitive characters inside TeX math strings
#'
#' When table output is rendered with `escape = FALSE`, symbols like `<` and
#' `>` in TeX expressions can be interpreted as HTML and break MathJax parsing.
#' This helper preserves TeX while making the string safe for HTML contexts.
#'
#' @param x A TeX math string.
#' @return A string with HTML-sensitive characters escaped.
#' @noRd
.escape_math_html <- function(x) {
  x <- gsub("&", "&amp;", x, fixed = TRUE)
  x <- gsub("<", "&lt;", x, fixed = TRUE)
  gsub(">", "&gt;", x, fixed = TRUE)
}


#' Build an image source URL appropriate for output format
#'
#' For HTML output, local files are embedded as base64 data URIs so that
#' viewer pages remain self-contained and do not depend on external files.
#'
#' @param path Character path to an image file.
#' @param format One of `"data.frame"`, `"html"`, or `"markdown"`.
#' @return A character string suitable for an `<img src="...">` attribute.
#' @noRd
.figure_src_for_format <- function(path, format) {
  if (!nzchar(path) || !file.exists(path)) {
    return(path)
  }

  if (format != "html") {
    return(path)
  }

  ext <- tolower(tools::file_ext(path))
  mime <- switch(ext,
    png  = "image/png",
    jpg  = "image/jpeg",
    jpeg = "image/jpeg",
    gif  = "image/gif",
    svg  = "image/svg+xml",
    webp = "image/webp",
    "application/octet-stream"
  )

  encoded <- base64enc::base64encode(path)
  sprintf("data:%s;base64,%s", mime, encoded)
}


#' Validate an optional Markdown figure directory
#'
#' @param figures_dir `NULL` or a scalar character directory.
#' @return A normalized directory string or `NULL`.
#' @noRd
.validate_figures_dir <- function(figures_dir) {
  if (is.null(figures_dir)) {
    return(NULL)
  }

  if (!is.character(figures_dir) || length(figures_dir) != 1L ||
      is.na(figures_dir) || !nzchar(figures_dir)) {
    stop("'figures_dir' must be NULL or a non-empty character string.",
      call. = FALSE
    )
  }

  sub("/+$", "", .forward_slash_path(figures_dir))
}


#' Copy Markdown figures to a durable document-local path
#'
#' @param df A data frame containing a `figure` column.
#' @param figures_dir Optional user-specified output directory. When `NULL`,
#'   the active knitr figure path is used during non-interactive rendering.
#' @return A copy of `df` with `figure` paths rewritten when files are copied.
#' @noRd
.copy_markdown_figures <- function(df, figures_dir = NULL) {
  figures <- as.character(df[["figure"]])
  has_figure <- !is.na(figures) & nzchar(figures) & file.exists(figures)

  if (!any(has_figure)) {
    return(df)
  }

  target <- .markdown_figure_target(figures_dir)
  if (is.null(target)) {
    return(df)
  }

  unique_sources <- unique(figures[has_figure])
  destinations <- .build_markdown_figure_destinations(
    sources = unique_sources,
    df = df,
    figures = figures,
    target = target
  )

  dirs <- unique(dirname(destinations$absolute))
  missing_dirs <- dirs[!dir.exists(dirs)]
  if (length(missing_dirs) > 0L) {
    ok <- vapply(
      missing_dirs,
      dir.create,
      logical(1),
      recursive = TRUE,
      showWarnings = FALSE
    )
    if (!all(ok)) {
      stop(
        "Could not create figure directory: ",
        paste(missing_dirs[!ok], collapse = ", "),
        call. = FALSE
      )
    }
  }

  copied <- .copy_markdown_figure_files(destinations)
  if (!all(copied)) {
    failed <- destinations$source[!copied]
    stop("Could not copy figure file(s): ", paste(failed, collapse = ", "),
      call. = FALSE
    )
  }

  rewritten <- stats::setNames(destinations$relative, destinations$source)
  df[["figure"]][has_figure] <- unname(rewritten[figures[has_figure]])
  df
}


#' Determine where Markdown figures should be copied
#'
#' @param figures_dir Optional user-specified output directory.
#' @return A list describing a directory target or knitr figure-prefix target,
#'   or `NULL` when automatic copying should not happen.
#' @noRd
.markdown_figure_target <- function(figures_dir = NULL) {
  if (!is.null(figures_dir)) {
    return(.manual_markdown_figure_target(figures_dir))
  }

  .knitr_markdown_figure_target()
}


#' Build a target from a user-specified figure directory
#'
#' @param figures_dir Figure output directory.
#' @return A directory target list.
#' @noRd
.manual_markdown_figure_target <- function(figures_dir) {
  output_dir <- .knitr_output_dir(default = getwd())

  absolute_dir <- if (.is_absolute_path(figures_dir)) {
    figures_dir
  } else {
    file.path(output_dir, figures_dir)
  }

  list(
    type = "directory",
    relative = figures_dir,
    absolute = absolute_dir
  )
}


#' Build a target from the active knitr figure path
#'
#' When `fig.path` ends with `/` it denotes a directory (figures are placed
#' inside it) and a `"directory"` target is returned.  Otherwise the path is
#' treated as a filename prefix and a `"prefix"` target is returned.
#'
#' Using `file.path()` for directory-type targets avoids a Windows-specific
#' issue where `file.path()` strips the trailing `/` from path components,
#' which would cause `paste0()` to concatenate the directory name and filename
#' without a separator.
#'
#' @return A knitr target list, or `NULL` outside a document render.
#' @noRd
.knitr_markdown_figure_target <- function() {
  if (!.is_noninteractive_document_render()) {
    return(NULL)
  }

  fig_path <- knitr::opts_current$get("fig.path")
  if (!is.character(fig_path) || length(fig_path) != 1L ||
      is.na(fig_path) || !nzchar(fig_path)) {
    return(NULL)
  }

  fig_path <- .forward_slash_path(fig_path)
  output_dir <- .knitr_output_dir(default = getwd())

  if (endsWith(fig_path, "/")) {
    dir_path <- sub("/+$", "", fig_path)
    absolute_dir <- if (.is_absolute_path(dir_path)) {
      dir_path
    } else {
      file.path(output_dir, dir_path)
    }
    return(list(
      type = "directory",
      relative = dir_path,
      absolute = absolute_dir
    ))
  }

  absolute_prefix <- if (.is_absolute_path(fig_path)) {
    fig_path
  } else {
    file.path(output_dir, fig_path)
  }

  list(
    type = "prefix",
    relative = fig_path,
    absolute = absolute_prefix
  )
}


#' Detect a non-interactive knitr render used by Quarto or R Markdown
#'
#' @return `TRUE` when the current session appears to be rendering a document.
#' @noRd
.is_noninteractive_document_render <- function() {
  if (!requireNamespace("knitr", quietly = TRUE)) {
    return(FALSE)
  }

  pandoc_to <- knitr::opts_knit$get("rmarkdown.pandoc.to")
  output_dir <- knitr::opts_knit$get("output.dir")

  .has_scalar_value(pandoc_to) || .has_scalar_value(output_dir)
}


#' Get knitr's output directory
#'
#' @param default Directory to use when knitr has no output directory.
#' @return A scalar character directory.
#' @noRd
.knitr_output_dir <- function(default = getwd()) {
  output_dir <- knitr::opts_knit$get("output.dir")
  if (.has_scalar_value(output_dir)) {
    return(output_dir)
  }

  default
}


#' Build Markdown figure destination paths
#'
#' @param sources Unique source image paths.
#' @param df Source table.
#' @param figures Original `figure` column.
#' @param target Figure target list.
#' @return A data frame of source, absolute destination, and relative paths.
#' @noRd
.build_markdown_figure_destinations <- function(sources, df, figures, target) {
  used_stems <- character(0)
  absolute <- character(length(sources))
  relative <- character(length(sources))

  for (i in seq_along(sources)) {
    source <- sources[[i]]
    row_index <- match(source, figures)
    label <- if ("term" %in% names(df)) df[["term"]][[row_index]] else source
    stem <- .unique_file_stem(
      .sanitize_file_stem(label, fallback = paste0("figure-", i)),
      used_stems
    )
    used_stems <- c(used_stems, stem)

    ext <- tolower(tools::file_ext(source))
    if (!nzchar(ext)) {
      ext <- "png"
    }

    file_name <- paste0(stem, ".", ext)
    relative[[i]] <- .target_figure_path(target$relative, file_name,
      target$type
    )
    absolute[[i]] <- .target_figure_path(target$absolute, file_name,
      target$type
    )
  }

  data.frame(
    source = sources,
    absolute = absolute,
    relative = .forward_slash_path(relative),
    stringsAsFactors = FALSE
  )
}


#' Build a figure path for a target
#'
#' @param target Directory or prefix.
#' @param file_name Figure file name.
#' @param type Either `"directory"` or `"prefix"`.
#' @return A character path.
#' @noRd
.target_figure_path <- function(target, file_name, type) {
  if (identical(type, "directory")) {
    return(file.path(target, file_name))
  }

  paste0(target, file_name)
}


#' Copy Markdown figure files
#'
#' @param destinations Destination data frame.
#' @return Logical vector indicating copy success.
#' @noRd
.copy_markdown_figure_files <- function(destinations) {
  copied <- logical(nrow(destinations))

  for (i in seq_len(nrow(destinations))) {
    source <- destinations$source[[i]]
    absolute <- destinations$absolute[[i]]

    same_file <- file.exists(absolute) &&
      identical(
        normalizePath(source, winslash = "/", mustWork = TRUE),
        normalizePath(absolute, winslash = "/", mustWork = TRUE)
      )

    copied[[i]] <- same_file || file.copy(
      from = source,
      to = absolute,
      overwrite = TRUE
    )
  }

  copied
}


#' Test whether a value is a non-empty scalar character
#'
#' @param x Object to check.
#' @return Logical scalar.
#' @noRd
.has_scalar_value <- function(x) {
  is.character(x) && length(x) == 1L && !is.na(x) && nzchar(x)
}


#' Test whether a path is absolute
#'
#' @param path Character path.
#' @return Logical scalar.
#' @noRd
.is_absolute_path <- function(path) {
  grepl("^(/|[A-Za-z]:/)", path)
}


#' Format a data frame as HTML, Markdown, or return as-is
#'
#' For `"html"` and `"markdown"`, the \pkg{knitr} package must be installed.
#' If it is not available, an error is raised.
#'
#' Math and figure columns are transformed to output-specific markup: HTML
#' output uses `$$...$$` math and `<img>` tags, while Markdown output uses
#' `$...$` math and Markdown image syntax.
#'
#' @param df A data frame to format.
#' @param format One of `"data.frame"`, `"html"`, or `"markdown"`.
#' @return The data frame (if `format = "data.frame"`) or a `knitr_kable`
#'   object.
#' @noRd
.format_output <- function(df, format, figures_dir = NULL) {
  if (format == "data.frame") {
    return(df)
  }

  if (!requireNamespace("knitr", quietly = TRUE)) {
    stop(
      sprintf(
        "The 'knitr' package is required for format = \"%s\". Install it with install.packages(\"knitr\").",
        format
      ),
      call. = FALSE
    )
  }

  notes <- if ("term" %in% names(df)) {
    .term_drawing_notes(df[["term"]])
  } else {
    character(0)
  }

  df <- .preprocess_columns(df, format, figures_dir = figures_dir)

  knitr_format <- switch(format,
    html     = "html",
    markdown = "pipe"
  )

  out <- knitr::kable(df, format = knitr_format, row.names = FALSE,
    escape = FALSE
  )
  .append_table_notes(out, notes, format)
}


#' Append drawing-convention notes below a formatted table
#'
#' When the figures in the table use drawing conventions (attribute
#' colors, mixing colors, or bipartite shapes), an explanatory note is
#' appended after the table so that readers can interpret the drawings.
#'
#' @param kable_obj A `knitr_kable` object.
#' @param notes Character vector of notes (possibly empty).
#' @param format Either `"html"` or `"markdown"`.
#' @return The `knitr_kable` object, with notes appended when present.
#' @noRd
.append_table_notes <- function(kable_obj, notes, format) {
  if (length(notes) == 0L) {
    return(kable_obj)
  }

  note_text <- paste(notes, collapse = " ")
  lines <- if (format == "markdown") {
    c(as.character(kable_obj), "", paste0("*Note: ", note_text, "*"))
  } else {
    c(
      as.character(kable_obj),
      sprintf('<p class="tabulergm-note"><em>Note: %s</em></p>', note_text)
    )
  }

  structure(lines, format = attr(kable_obj, "format"), class = "knitr_kable")
}


# ---- tabulergm_view: open table in viewer or browser ----

#' View an ERGM Table in the RStudio Viewer or System Browser
#'
#' Builds a self-contained HTML page containing the formatted ERGM table
#' (with MathJax for LaTeX math rendering) and opens it in the RStudio
#' viewer pane when available, falling back to [utils::browseURL()].
#'
#' @param object A fitted [ergm][ergm::ergm] object or an ERGM
#'   [formula][stats::formula].
#' @param ... Additional arguments passed to [tabulergm_table()].
#' @return Invisibly returns the path to the temporary HTML file.
#' @export
#' @seealso [tabulergm_table()]
#' @examples
#' \dontrun{
#' library(ergm)
#' fit <- readRDS(system.file("fits", "fit_edges.rds", package = "tabulergm"))
#' tabulergm_view(fit)
#'
#' # Also works with a formula (shows term metadata only)
#' tabulergm_view(network ~ edges + triangle)
#' }
tabulergm_view <- function(object, ...) {
  UseMethod("tabulergm_view")
}

#' @describeIn tabulergm_view Method for fitted ergm objects.
#' @export
tabulergm_view.ergm <- function(object, ...) {
  tbl <- tabulergm_table(object, format = "html", ...)
  .open_html_viewer(tbl)
}

#' @describeIn tabulergm_view Method for formula objects.
#' @export
tabulergm_view.formula <- function(object, ...) {
  tbl <- tabulergm_table(object, format = "html", ...)
  .open_html_viewer(tbl)
}


#' Build a self-contained HTML document and open it in a viewer
#'
#' Wraps the supplied `knitr_kable` HTML table in a minimal HTML page that
#' loads MathJax from a CDN for LaTeX rendering, writes it to a temporary
#' file, and opens it.
#'
#' @param tbl A `knitr_kable` object (html format) or any character that
#'   can be coerced to a string.
#' @return Invisibly returns the path to the temporary HTML file.
#' @noRd
.open_html_viewer <- function(tbl) {
  html_table <- paste(as.character(tbl), collapse = "\n")

  html_content <- paste0(
    "<!DOCTYPE html>\n",
    "<html>\n<head>\n",
    "<meta charset=\"UTF-8\">\n",
    "<title>ERGM Table</title>\n",
    "<script src=\"https://cdn.jsdelivr.net/npm/mathjax@3/es5/tex-chtml.js\"></script>\n",
    "<style>\n",
    "body{font-family:sans-serif;margin:2em;}\n",
    "table{border-collapse:collapse;width:100%;}\n",
    "th,td{border:1px solid #ddd;padding:8px;text-align:left;vertical-align:middle;}\n",
    "th{background-color:#f2f2f2;}\n",
    "img{display:block;}\n",
    "</style>\n",
    "</head>\n<body>\n",
    html_table, "\n",
    "</body>\n</html>"
  )

  tmp_file <- tempfile(fileext = ".html")
  writeLines(html_content, tmp_file)

  if (!interactive()) {
    return(invisible(tmp_file))
  }

  if (requireNamespace("rstudioapi", quietly = TRUE) &&
        rstudioapi::isAvailable()) {
    rstudioapi::viewer(tmp_file)
  } else {
    utils::browseURL(tmp_file)
  }

  invisible(tmp_file)
}
