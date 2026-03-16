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
#' \dontrun{
#' library(ergm)
#' data(florentine)
#' fit <- ergm(flomarriage ~ edges + nodematch("wealth"))
#' tabulergm_table(fit)
#' tabulergm_table(fit, include_description = TRUE)
#' tabulergm_table(fit, format = "markdown")
#' }
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
#' \dontrun{
#' library(ergm)
#' tabulergm_table(network ~ edges + nodematch("gender"))
#' }
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

#' Format a data frame as HTML, Markdown, or return as-is
#'
#' For `"html"` and `"markdown"`, the \pkg{knitr} package must be installed.
#' If it is not available, an error is raised.
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

  knitr_format <- switch(format,
    html     = "html",
    markdown = "pipe"
  )

  knitr::kable(df, format = knitr_format, row.names = FALSE)
}
