#' Save a tabulergm Table and Figure Assets
#'
#' Saves the table code produced by `tabulergm` and copies the generated term
#' figures out of the temporary cache into a user-controlled directory. The
#' exported table references the copied image files with relative paths, making
#' the result easier to reuse in another document or project.
#'
#' @param object A fitted [ergm][ergm::ergm] object, an ERGM
#'   [formula][stats::formula], or a `data.frame` returned by
#'   [tabulergm_table()].
#' @param path Target directory. It is created recursively when needed.
#' @param format Character vector of output formats to write. Supported values
#'   are `"markdown"` and `"latex"`. By default both are written.
#' @param filename File name stem for the table code files, without extension.
#'   Defaults to `"tabulergm-table"`.
#' @param images_dir Directory, relative to `path`, where copied images should
#'   be stored. Use `"."` to place images directly in `path`.
#' @param overwrite Logical. Overwrite existing table code and image files?
#'   Default `TRUE`.
#' @param latex_image_width Character width passed to `\\includegraphics` in
#'   LaTeX output. Default `"0.7in"`.
#' @param ... For `ergm` and `formula` methods, additional arguments passed to
#'   [tabulergm_table()]. For the `data.frame` method, additional arguments are
#'   ignored.
#'
#' @return Invisibly returns a list with:
#' \describe{
#'   \item{path}{The normalized export directory.}
#'   \item{files}{Named character vector of written table code files.}
#'   \item{figures}{Named character vector of copied figure files, where names
#'     are the relative paths used in the table.}
#'   \item{table}{The data frame used for export, with `figure` paths rewritten
#'     to the copied relative paths.}
#' }
#'
#' @export
#' @seealso [tabulergm_table()]
#' @examples
#' if (requireNamespace("knitr", quietly = TRUE)) {
#'   fit <- readRDS(system.file("fits", "fit_edges.rds", package = "tabulergm"))
#'   out_dir <- tempfile("tabulergm-export-")
#'   tabulergm_save(fit, out_dir, include_math = TRUE)
#' }
tabulergm_save <- function(object, path, ...) {
  UseMethod("tabulergm_save", object)
}

#' @describeIn tabulergm_save Method for fitted ergm objects.
#' @export
tabulergm_save.ergm <- function(
    object,
    path,
    format = c("markdown", "latex"),
    filename = "tabulergm-table",
    images_dir = "figures",
    overwrite = TRUE,
    latex_image_width = "0.7in",
    ...) {

  tbl <- tabulergm_table(object, ...)
  tabulergm_save.data.frame(
    tbl,
    path = path,
    format = format,
    filename = filename,
    images_dir = images_dir,
    overwrite = overwrite,
    latex_image_width = latex_image_width
  )
}

#' @describeIn tabulergm_save Method for formula objects.
#' @export
tabulergm_save.formula <- function(
    object,
    path,
    format = c("markdown", "latex"),
    filename = "tabulergm-table",
    images_dir = "figures",
    overwrite = TRUE,
    latex_image_width = "0.7in",
    ...) {

  tbl <- tabulergm_table(object, ...)
  tabulergm_save.data.frame(
    tbl,
    path = path,
    format = format,
    filename = filename,
    images_dir = images_dir,
    overwrite = overwrite,
    latex_image_width = latex_image_width
  )
}

#' @describeIn tabulergm_save Method for data frames returned by
#'   `tabulergm_table()`.
#' @export
tabulergm_save.data.frame <- function(
    object,
    path,
    format = c("markdown", "latex"),
    filename = "tabulergm-table",
    images_dir = "figures",
    overwrite = TRUE,
    latex_image_width = "0.7in",
    ...) {

  if (!requireNamespace("knitr", quietly = TRUE)) {
    stop(
      "The 'knitr' package is required to save table code. ",
      "Install it with install.packages(\"knitr\").",
      call. = FALSE
    )
  }

  format <- unique(match.arg(format, c("markdown", "latex"), several.ok = TRUE))
  path <- .prepare_save_directory(path)
  filename <- .validate_save_filename(filename)
  images_dir <- .validate_images_dir(images_dir)
  overwrite <- .validate_save_flag(overwrite, "overwrite")
  latex_image_width <- .validate_scalar_character(
    latex_image_width,
    "latex_image_width"
  )

  saved <- .copy_save_figures(
    object,
    path = path,
    images_dir = images_dir,
    overwrite = overwrite
  )

  files <- .write_save_tables(
    saved$table,
    path = path,
    format = format,
    filename = filename,
    overwrite = overwrite,
    latex_image_width = latex_image_width
  )

  invisible(list(
    path = normalizePath(path, winslash = "/", mustWork = TRUE),
    files = files,
    figures = saved$figures,
    table = saved$table
  ))
}

#' @export
tabulergm_save.default <- function(object, path, ...) {
  stop(
    "'object' must be an ergm object, a formula, or a data frame returned by ",
    "tabulergm_table().",
    call. = FALSE
  )
}


# ---- Internal Helpers: save validation ----

.prepare_save_directory <- function(path) {
  path <- .validate_scalar_character(path, "path")

  if (file.exists(path) && !dir.exists(path)) {
    stop("'path' exists but is not a directory.", call. = FALSE)
  }

  if (!dir.exists(path)) {
    ok <- dir.create(path, recursive = TRUE, showWarnings = FALSE)
    if (!ok) {
      stop("Could not create directory: ", path, call. = FALSE)
    }
  }

  path
}

.validate_save_filename <- function(filename) {
  filename <- .validate_scalar_character(filename, "filename")

  if (!identical(dirname(filename), ".")) {
    stop("'filename' must be a file name stem, not a path.", call. = FALSE)
  }

  filename <- sub("\\.(md|tex)$", "", filename, ignore.case = TRUE)
  if (!nzchar(filename)) {
    stop("'filename' must not be empty.", call. = FALSE)
  }

  filename
}

.validate_images_dir <- function(images_dir) {
  images_dir <- .validate_scalar_character(images_dir, "images_dir")
  images_dir <- sub("/+$", "", gsub("\\\\", "/", images_dir))

  if (!nzchar(images_dir)) {
    images_dir <- "."
  }

  if (grepl("^(/|[A-Za-z]:/)", images_dir)) {
    stop("'images_dir' must be relative to 'path'.", call. = FALSE)
  }

  parts <- strsplit(images_dir, "/", fixed = TRUE)[[1L]]
  if (any(parts == "..")) {
    stop("'images_dir' must not contain '..'.", call. = FALSE)
  }

  images_dir
}

.validate_save_flag <- function(x, name) {
  if (!is.logical(x) || length(x) != 1L || is.na(x)) {
    stop("'", name, "' must be TRUE or FALSE.", call. = FALSE)
  }

  x
}

.validate_scalar_character <- function(x, name) {
  if (!is.character(x) || length(x) != 1L || is.na(x) || !nzchar(x)) {
    stop("'", name, "' must be a non-empty character string.", call. = FALSE)
  }

  x
}


# ---- Internal Helpers: figure copying ----

.copy_save_figures <- function(df, path, images_dir, overwrite) {
  if (!"figure" %in% names(df)) {
    return(list(table = df, figures = character(0)))
  }

  figures <- as.character(df[["figure"]])
  has_figure <- !is.na(figures) & nzchar(figures) & file.exists(figures)

  df[["figure"]][!has_figure] <- ""
  if (!any(has_figure)) {
    return(list(table = df, figures = character(0)))
  }

  image_path <- if (identical(images_dir, ".")) {
    path
  } else {
    file.path(path, images_dir)
  }

  if (!dir.exists(image_path)) {
    ok <- dir.create(image_path, recursive = TRUE, showWarnings = FALSE)
    if (!ok) {
      stop("Could not create image directory: ", image_path, call. = FALSE)
    }
  }

  unique_sources <- unique(figures[has_figure])
  destinations <- .build_figure_destinations(
    sources = unique_sources,
    df = df,
    figures = figures,
    path = path,
    images_dir = images_dir
  )

  existing <- destinations$absolute[file.exists(destinations$absolute)]
  if (!overwrite && length(existing) > 0L) {
    stop(
      "File(s) already exist and 'overwrite' is FALSE: ",
      paste(existing, collapse = ", "),
      call. = FALSE
    )
  }

  copied <- file.copy(
    from = destinations$source,
    to = destinations$absolute,
    overwrite = overwrite
  )

  if (!all(copied)) {
    failed <- destinations$source[!copied]
    stop("Could not copy figure file(s): ", paste(failed, collapse = ", "),
      call. = FALSE
    )
  }

  rewritten <- stats::setNames(destinations$relative, destinations$source)
  df[["figure"]][has_figure] <- unname(rewritten[figures[has_figure]])

  saved_figures <- normalizePath(
    destinations$absolute,
    winslash = "/",
    mustWork = TRUE
  )
  names(saved_figures) <- destinations$relative

  list(table = df, figures = saved_figures)
}

.build_figure_destinations <- function(sources, df, figures, path, images_dir) {
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
    relative[[i]] <- if (identical(images_dir, ".")) {
      file_name
    } else {
      file.path(images_dir, file_name)
    }
    absolute[[i]] <- file.path(path, relative[[i]])
  }

  data.frame(
    source = sources,
    absolute = absolute,
    relative = .forward_slash_path(relative),
    stringsAsFactors = FALSE
  )
}

.sanitize_file_stem <- function(x, fallback) {
  x <- as.character(x)[[1L]]
  if (is.na(x)) {
    x <- fallback
  }

  x <- tolower(trimws(x))
  x <- gsub("[^A-Za-z0-9]+", "-", x)
  x <- gsub("^-+|-+$", "", x)

  if (!nzchar(x)) fallback else x
}

.unique_file_stem <- function(stem, used_stems) {
  if (!stem %in% used_stems) {
    return(stem)
  }

  i <- 2L
  candidate <- paste0(stem, "-", i)
  while (candidate %in% used_stems) {
    i <- i + 1L
    candidate <- paste0(stem, "-", i)
  }

  candidate
}

.forward_slash_path <- function(path) {
  gsub("\\\\", "/", path)
}


# ---- Internal Helpers: table rendering ----

.write_save_tables <- function(
    df,
    path,
    format,
    filename,
    overwrite,
    latex_image_width) {

  extensions <- c(markdown = "md", latex = "tex")
  output_paths <- file.path(path, paste0(filename, ".", extensions[format]))
  names(output_paths) <- format

  existing <- output_paths[file.exists(output_paths)]
  if (!overwrite && length(existing) > 0L) {
    stop(
      "File(s) already exist and 'overwrite' is FALSE: ",
      paste(existing, collapse = ", "),
      call. = FALSE
    )
  }

  for (fmt in format) {
    code <- switch(fmt,
      markdown = .render_save_markdown(df),
      latex = .render_save_latex(df, latex_image_width = latex_image_width)
    )
    writeLines(code, output_paths[[fmt]], useBytes = TRUE)
  }

  normalized <- normalizePath(output_paths, winslash = "/", mustWork = TRUE)
  names(normalized) <- names(output_paths)
  normalized
}

.render_save_markdown <- function(df) {
  notes <- .save_table_notes(df)
  df <- .preprocess_columns(df, "markdown", copy_figures = FALSE)
  code <- as.character(knitr::kable(df, format = "pipe", row.names = FALSE,
    escape = FALSE
  ))
  if (length(notes) > 0L) {
    code <- c(code, "", paste0("*Note: ", paste(notes, collapse = " "), "*"))
  }
  code
}

.render_save_latex <- function(df, latex_image_width) {
  notes <- .save_table_notes(df)
  df <- .preprocess_latex_columns(df, latex_image_width = latex_image_width)
  code <- as.character(knitr::kable(df, format = "latex", row.names = FALSE,
    escape = FALSE
  ))
  if (length(notes) > 0L) {
    code <- c(
      code,
      "",
      sprintf("\\emph{Note: %s}", paste(notes, collapse = " "))
    )
  }
  code
}

.save_table_notes <- function(df) {
  if (!"term" %in% names(df)) {
    return(character(0))
  }
  .term_drawing_notes(df[["term"]])
}

.preprocess_latex_columns <- function(df, latex_image_width) {
  if ("math" %in% names(df)) {
    has_math <- !is.na(df[["math"]]) & nzchar(trimws(df[["math"]]))
    df[["math"]][has_math] <- paste0("$", trimws(df[["math"]][has_math]), "$")
    df[["math"]][!has_math] <- ""
  }

  if ("figure" %in% names(df)) {
    has_fig <- !is.na(df[["figure"]]) & nzchar(df[["figure"]])
    figure_path <- .forward_slash_path(df[["figure"]][has_fig])
    df[["figure"]][has_fig] <- sprintf(
      "\\includegraphics[width=%s]{%s}",
      latex_image_width,
      figure_path
    )
    df[["figure"]][!has_fig] <- ""
  }

  text_cols <- vapply(df, is.character, logical(1))
  text_cols[names(text_cols) %in% c("figure", "math")] <- FALSE
  df[text_cols] <- lapply(df[text_cols], .escape_latex_text)

  df
}

.escape_latex_text <- function(x) {
  missing <- is.na(x)
  x[missing] <- ""

  replacements <- c(
    "\\" = "\\textbackslash{}",
    "#"  = "\\#",
    "$"  = "\\$",
    "%"  = "\\%",
    "&"  = "\\&",
    "_"  = "\\_",
    "{"  = "\\{",
    "}"  = "\\}",
    "~"  = "\\textasciitilde{}",
    "^"  = "\\textasciicircum{}"
  )

  x <- vapply(x, function(value) {
    chars <- strsplit(value, "", useBytes = TRUE)[[1L]]
    chars <- ifelse(chars %in% names(replacements), replacements[chars], chars)
    paste0(chars, collapse = "")
  }, character(1), USE.NAMES = FALSE)

  x[missing] <- ""
  x
}
