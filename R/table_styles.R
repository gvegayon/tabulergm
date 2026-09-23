# ---- Table styles -----------------------------------------------------------

#' Restore the Plain tabulergm Table Style
#'
#' Restores the default column layout of a table returned by
#' [tabulergm_table()]. This is useful after applying another table style in a
#' pipe.
#'
#' @param x A data frame or `knitr_kable` object returned by
#'   [tabulergm_table()] or one of the `with_style_*()` helpers.
#' @return An object in the same requested output format as `x`.
#' @export
#' @examples
#' tbl <- tabulergm_table(~ edges)
#' tbl |> with_style_name_over_formula() |> with_style_plain()
with_style_plain <- function(x) {
  spec <- .table_spec_from(x)
  spec$style <- "plain"
  .render_table_spec(spec)
}


#' Stack Term Names over Mathematical Formulae
#'
#' Reorganizes a [tabulergm_table()] result into a compact presentation layout.
#' The `Name` column contains the curated term title (or the canonical term
#' name when no title is available) followed by its formula; `Representation`
#' contains the term figure. Model statistics and metadata columns explicitly
#' included in the source table are retained after those two presentation
#' columns.
#'
#' HTML output uses a responsive HTML table. Markdown requests also use HTML,
#' because pipe tables cannot reliably represent the multi-line cells needed
#' by this style. Width and figure-height settings are stored on the returned
#' table object, so they also apply to previews and saved output. The saved
#' LaTeX snippet requires the `array`, `booktabs`, and `graphicx` packages.
#'
#' @param x A data frame or `knitr_kable` object returned by
#'   [tabulergm_table()] or one of the `with_style_*()` helpers.
#' @param column_widths Optional named numeric vector of table-width fractions
#'   keyed by displayed compact-table column names (for example,
#'   `c(Name = .5, Representation = .2)`). Values must be in `(0, 1]` and
#'   total no more than one. `NULL` uses automatic column widths.
#' @param figure_height Optional positive numeric height, in inches, for term
#'   figures. `NULL` retains the default width-based figure sizing.
#' @return An object in the same requested output format as `x`; Markdown
#'   requests return HTML table markup suitable for HTML-capable Markdown
#'   renderers.
#' @export
#' @examples
#' tabulergm_table(~ edges + triangle, include_description = FALSE) |>
#'   with_style_name_over_formula(
#'     column_widths = c(Name = .5, Representation = .2),
#'     figure_height = .8
#'   )
with_style_name_over_formula <- function(
    x,
    column_widths = NULL,
    figure_height = NULL) {
  spec <- .table_spec_from(x)
  if (missing(column_widths)) {
    column_widths <- spec$layout$column_widths
  } else {
    column_widths <- .validate_column_widths(
      column_widths,
      .name_over_formula_column_names(spec)
    )
  }
  if (missing(figure_height)) {
    figure_height <- spec$layout$figure_height
  } else {
    figure_height <- .validate_figure_height(figure_height)
  }

  spec$layout$column_widths <- column_widths
  spec$layout$figure_height <- figure_height
  spec$style <- "name_over_formula"
  .render_table_spec(spec)
}


# ---- Internal table specification ------------------------------------------

.new_table_spec <- function(data, parsed, format, figures_dir = NULL,
                            digits = 2L) {
  list(
    data = data,
    parsed = parsed,
    format = format,
    # Normalized once so the plain route (.preprocess_columns()) and the
    # styled route (.format_name_over_formula()) resolve the same directory.
    figures_dir = .validate_figures_dir(figures_dir),
    style = "plain",
    digits = .validate_table_digits(digits),
    layout = list(column_widths = NULL, figure_height = NULL)
  )
}

.table_spec_from <- function(x) {
  spec <- attr(x, "tabulergm_spec", exact = TRUE)
  if (is.null(spec) || !is.list(spec) || is.null(spec$data) ||
      is.null(spec$parsed)) {
    stop(
      "'x' must be a table returned by tabulergm_table() or with_style_*().",
      call. = FALSE
    )
  }
  if (!"digits" %in% names(spec)) spec$digits <- 2L
  if (!"layout" %in% names(spec) || !is.list(spec$layout)) {
    spec$layout <- list()
  }
  if (!"column_widths" %in% names(spec$layout)) {
    spec$layout$column_widths <- NULL
  }
  if (!"figure_height" %in% names(spec$layout)) {
    spec$layout$figure_height <- NULL
  }
  spec
}

.attach_table_spec <- function(x, spec, citations = NULL, table_class = NULL) {
  attr(x, "tabulergm_spec") <- spec
  if (!is.null(citations)) attr(x, "tabulergm_citations") <- citations
  if (!is.null(table_class)) {
    class(x) <- unique(c(table_class, class(x)))
  }
  x
}

.render_table_spec <- function(spec) {
  display <- .materialize_table_spec(spec)
  .format_output(
    display$df,
    format = spec$format,
    figures_dir = spec$figures_dir,
    citations = display$citations,
    spec = spec,
    display = display
  )
}

.materialize_table_spec <- function(spec) {
  switch(spec$style,
    plain = {
      marked <- .apply_citation_markers(
        .apply_table_digits(spec$data, spec$digits), spec$parsed
      )
      list(
        df = marked$df,
        labels = NULL,
        math = NULL,
        figures = NULL,
        terms = as.character(spec$parsed[["term"]]),
        citations = marked$citations
      )
    },
    name_over_formula = .materialize_name_over_formula(spec),
    stop("Unknown tabulergm table style.", call. = FALSE)
  )
}

.materialize_name_over_formula <- function(spec) {
  data <- .apply_table_digits(spec$data, spec$digits)
  parsed <- spec$parsed
  labels <- as.character(parsed[["title"]])
  terms <- as.character(parsed[["term"]])
  missing_title <- is.na(labels) | !nzchar(trimws(labels))
  labels[missing_title] <- terms[missing_title]

  math <- as.character(parsed[["math"]])
  math[is.na(math)] <- ""
  figures <- as.character(parsed[["figure"]])
  figures[is.na(figures)] <- ""

  keep <- setdiff(names(data), c("term", "title", "math", "figure"))
  df <- data.frame(
    Name = labels,
    Representation = figures,
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  if (length(keep) > 0L) df <- cbind(df, data[, keep, drop = FALSE])

  keys <- parsed[["citation"]]
  bib <- attr(parsed, "tabulergm_citations")
  used <- character(0)
  target <- if ("description" %in% names(df)) "description" else "Name"
  if (!is.null(keys)) for (i in which(!is.na(keys) & nzchar(keys))) {
    row_keys <- trimws(strsplit(keys[[i]], ",", fixed = TRUE)[[1L]])
    row_keys <- row_keys[nzchar(row_keys)]
    if (length(row_keys) == 0L) next

    column <- target
    if (identical(column, "description")) {
      value <- df[[column]][[i]]
      if (is.na(value) || !nzchar(trimws(value))) column <- "Name"
    }
    value <- df[[column]][[i]]
    if (is.na(value)) value <- ""
    df[[column]][[i]] <- paste0(
      trimws(value), " (", paste(row_keys, collapse = "; "), ")"
    )
    used <- c(used, row_keys)
  }

  display_labels <- as.character(df[["Name"]])
  has_math <- nzchar(trimws(math))
  df[["Name"]][has_math] <- paste(
    display_labels[has_math], math[has_math], sep = "\n"
  )

  list(
    df = df,
    labels = display_labels,
    math = math,
    figures = figures,
    terms = terms,
    citations = if (is.null(bib)) list() else Filter(
      function(e) e[["key"]] %in% unique(used), bib
    )
  )
}

.apply_table_digits <- function(data, digits) {
  if (is.null(digits)) return(data)

  for (column in names(data)) {
    if (!is.numeric(data[[column]])) next
    data[[column]] <- if (identical(column, "pvalue")) {
      .format_pvalues(data[[column]], digits)
    } else {
      round(data[[column]], digits)
    }
  }
  data
}

# Formats p-values as fixed-decimal strings. Values that would round to zero
# are shown as an upper bound instead (e.g. "<0.01" for two digits) so a
# small p-value never reads as exactly zero.
.format_pvalues <- function(p, digits) {
  out <- formatC(p, format = "f", digits = digits)
  threshold <- 10^(-digits)
  small <- !is.na(p) & p < threshold
  out[small] <- paste0("<", formatC(threshold, format = "f", digits = digits))
  out[is.na(p)] <- NA_character_
  out
}

# p-values are formatted as strings but are still numbers to the reader, so
# they keep numeric (right) alignment.
.is_numeric_display_column <- function(df, column) {
  is.numeric(df[[column]]) || identical(column, "pvalue")
}

.kable_align <- function(df) {
  vapply(names(df), function(column) {
    if (.is_numeric_display_column(df, column)) "r" else "l"
  }, character(1), USE.NAMES = FALSE)
}

# "<0.01" is passed to HTML and Markdown with escape = FALSE.
.escape_pvalue_html <- function(df) {
  if ("pvalue" %in% names(df) && is.character(df[["pvalue"]])) {
    df[["pvalue"]] <- gsub("<", "&lt;", df[["pvalue"]], fixed = TRUE)
  }
  df
}

.validate_table_digits <- function(digits) {
  if (is.null(digits)) return(NULL)
  if (!is.numeric(digits) || length(digits) != 1L || is.na(digits) ||
      !is.finite(digits) || digits < 0 || digits != floor(digits)) {
    stop("'digits' must be NULL or a non-negative whole number.", call. = FALSE)
  }
  as.integer(digits)
}

.name_over_formula_column_names <- function(spec) {
  c(
    "Name", "Representation",
    setdiff(names(spec$data), c("term", "title", "math", "figure"))
  )
}

.validate_column_widths <- function(column_widths, columns) {
  if (is.null(column_widths)) return(NULL)
  if (!is.numeric(column_widths) || length(column_widths) == 0L ||
      is.null(names(column_widths)) || any(!nzchar(names(column_widths))) ||
      anyDuplicated(names(column_widths))) {
    stop(
      "'column_widths' must be a non-empty named numeric vector with unique names.",
      call. = FALSE
    )
  }
  if (any(!is.finite(column_widths)) || any(column_widths <= 0) ||
      any(column_widths > 1) || sum(column_widths) > 1) {
    stop(
      "'column_widths' values must be finite fractions in (0, 1] that total no more than one.",
      call. = FALSE
    )
  }
  unknown <- setdiff(names(column_widths), columns)
  if (length(unknown) > 0L) {
    stop(
      "'column_widths' names must match compact-table columns: ",
      paste(columns, collapse = ", "), ".",
      call. = FALSE
    )
  }
  column_widths
}

.validate_figure_height <- function(figure_height) {
  if (is.null(figure_height)) return(NULL)
  if (!is.numeric(figure_height) || length(figure_height) != 1L ||
      is.na(figure_height) || !is.finite(figure_height) || figure_height <= 0) {
    stop("'figure_height' must be NULL or one positive number in inches.", call. = FALSE)
  }
  as.numeric(figure_height)
}

.format_inches <- function(x) {
  paste0(format(x, trim = TRUE, scientific = FALSE), "in")
}


# ---- Compact HTML renderer --------------------------------------------------

.format_name_over_formula <- function(display, requested_format, figures_dir,
                                      citations, spec) {
  df <- display$df
  figure_df <- data.frame(
    term = display$terms,
    figure = display$figures,
    stringsAsFactors = FALSE
  )

  if (identical(requested_format, "markdown")) {
    figure_df <- .copy_markdown_figures(figure_df, figures_dir = figures_dir)
    sources <- as.character(figure_df[["figure"]])
  } else {
    sources <- vapply(display$figures, .figure_src_for_format,
      FUN.VALUE = character(1), format = "html"
    )
  }

  has_figure <- nzchar(sources)
  df[["Representation"]] <- ""
  figure_style <- if (is.null(spec$layout$figure_height)) {
    "width:40%;max-width:100%;"
  } else {
    paste0(
      "height:", .format_inches(spec$layout$figure_height),
      ";width:auto;max-width:100%;"
    )
  }
  df[["Representation"]][has_figure] <- sprintf(
    '<img src="%s" style="%s" alt="term figure">',
    sources[has_figure], figure_style
  )

  has_math <- nzchar(trimws(display$math))
  labels <- vapply(display$labels, .escape_html_text, character(1))
  escape_math <- if (identical(requested_format, "markdown")) {
    .escape_math_raw_html_markdown
  } else {
    .escape_math_html
  }
  df[["Name"]] <- labels
  df[["Name"]][has_math] <- paste0(
    labels[has_math], "<br><span class=\"tabulergm-formula\">$$",
    vapply(display$math[has_math], escape_math, character(1)),
    "$$</span>"
  )

  df <- .escape_pvalue_html(df)
  out <- knitr::kable(df, format = "html", row.names = FALSE, escape = FALSE,
    align = .kable_align(df)
  )
  lines <- as.character(out)
  colgroup <- .compact_html_colgroup(names(df), spec$layout$column_widths)
  lines <- sub(
    "<table>",
    paste0(
      '<table class="tabulergm-table tabulergm-style-name-over-formula">',
      colgroup
    ),
    lines,
    fixed = TRUE
  )
  lines <- gsub(
    '<td style="text-align:left;"> <img',
    '<td class="tabulergm-representation" style="text-align:center;"> <img',
    lines,
    fixed = TRUE
  )
  out <- structure(lines, format = "html", class = "knitr_kable")
  .append_table_notes(out,
    notes = .term_drawing_notes(display$terms),
    format = "html",
    citation_notes = .render_citation_notes(citations, "html"),
    spec = spec
  )
}

.compact_html_colgroup <- function(columns, column_widths) {
  if (is.null(column_widths)) return("")

  cols <- vapply(columns, function(column) {
    width <- column_widths[column]
    if (length(width) == 0L || is.na(width)) {
      "<col>"
    } else {
      paste0(
        '<col style="width:', format(100 * width, trim = TRUE,
          scientific = FALSE), '%;">'
      )
    }
  }, character(1))
  paste0("<colgroup>", paste0(cols, collapse = ""), "</colgroup>")
}

.escape_html_text <- function(x) {
  x <- gsub("&", "&amp;", x, fixed = TRUE)
  x <- gsub("<", "&lt;", x, fixed = TRUE)
  x <- gsub(">", "&gt;", x, fixed = TRUE)
  gsub('"', "&quot;", x, fixed = TRUE)
}
