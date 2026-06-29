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
#'   `format` is `"html"` or `"markdown"`.
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

  .format_output(result, format)
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
    ...) {

  format <- match.arg(format)

  parsed <- parse_ergm_formula(object)

  # Formula-only columns (no coefficient statistics)
  cols <- c("term", "figure", "math", "description")

  result <- parsed[, cols, drop = FALSE]
  rownames(result) <- NULL

  .format_output(result, format)
}


# ---- Internal Helpers: Output Formatting ----

#' Pre-process math and figure columns for formatted output
#'
#' For `"markdown"` and `"html"` formats:
#' * `math` values are wrapped in `$$...$$` (display LaTeX math).
#' * `figure` file-paths are converted to `<img>` HTML tags so that the
#'   images render when the table is viewed in a browser or knitted document.
#'
#' @param df A data frame containing the table.
#' @param format One of `"data.frame"`, `"html"`, or `"markdown"`.
#' @return A modified copy of `df`.
#' @noRd
.preprocess_columns <- function(df, format) {
  if (format == "data.frame") return(df)

  # Math column: wrap non-NA values in display-math delimiters
  if ("math" %in% names(df)) {
    has_math <- !is.na(df[["math"]]) & nzchar(trimws(df[["math"]]))
    df[["math"]][has_math] <- paste0(
      "$$", trimws(df[["math"]][has_math]), "$$"
    )
  }

  # Figure column: convert file paths to <img> tags
  if ("figure" %in% names(df)) {
    has_fig <- !is.na(df[["figure"]]) & nzchar(df[["figure"]])
    df[["figure"]][has_fig] <- sprintf(
      '<img src="%s" style="width:80px;height:80px;" alt="term figure">',
      df[["figure"]][has_fig]
    )
    df[["figure"]][!has_fig] <- ""
  }

  df
}


#' Format a data frame as HTML, Markdown, or return as-is
#'
#' For `"html"` and `"markdown"`, the \pkg{knitr} package must be installed.
#' If it is not available, an error is raised.
#'
#' Math column values are wrapped in `$$...$$` and figure file paths are
#' converted to `<img>` HTML tags so that images render properly.
#'
#' @param df A data frame to format.
#' @param format One of `"data.frame"`, `"html"`, or `"markdown"`.
#' @return The data frame (if `format = "data.frame"`) or a `knitr_kable`
#'   object.
#' @noRd
.format_output <- function(df, format) {
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

  df <- .preprocess_columns(df, format)

  knitr_format <- switch(format,
    html     = "html",
    markdown = "pipe"
  )

  knitr::kable(df, format = knitr_format, row.names = FALSE, escape = FALSE)
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

  if (requireNamespace("rstudioapi", quietly = TRUE) &&
        rstudioapi::isAvailable()) {
    rstudioapi::viewer(tmp_file)
  } else {
    utils::browseURL(tmp_file)
  }

  invisible(tmp_file)
}
