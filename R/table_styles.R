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
#' by this style. The saved LaTeX snippet requires the `array`, `booktabs`, and
#' `graphicx` packages.
#'
#' @param x A data frame or `knitr_kable` object returned by
#'   [tabulergm_table()] or one of the `with_style_*()` helpers.
#' @return An object in the same requested output format as `x`; Markdown
#'   requests return HTML table markup suitable for HTML-capable Markdown
#'   renderers.
#' @export
#' @examples
#' tabulergm_table(~ edges + triangle, include_description = FALSE) |>
#'   with_style_name_over_formula()
with_style_name_over_formula <- function(x) {
  spec <- .table_spec_from(x)
  spec$style <- "name_over_formula"
  .render_table_spec(spec)
}


# ---- Internal table specification ------------------------------------------

.new_table_spec <- function(data, parsed, format, figures_dir = NULL) {
  list(
    data = data,
    parsed = parsed,
    format = format,
    figures_dir = figures_dir,
    style = "plain"
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
      marked <- .apply_citation_markers(spec$data, spec$parsed)
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
  data <- spec$data
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
  df[["Representation"]][has_figure] <- sprintf(
    '<img src="%s" style="width:40%%;max-width:100%%;" alt="term figure">',
    sources[has_figure]
  )

  has_math <- nzchar(trimws(display$math))
  labels <- vapply(display$labels, .escape_html_text, character(1))
  df[["Name"]] <- labels
  df[["Name"]][has_math] <- paste0(
    labels[has_math], "<br><span class=\"tabulergm-formula\">$$",
    vapply(display$math[has_math], .escape_math_html, character(1)),
    "$$</span>"
  )

  out <- knitr::kable(df, format = "html", row.names = FALSE, escape = FALSE)
  lines <- as.character(out)
  lines <- sub(
    "<table>",
    '<table class="tabulergm-table tabulergm-style-name-over-formula">',
    lines,
    fixed = TRUE
  )
  lines <- sub(
    "<th style=\"text-align:left;\"> Name </th>",
    '<th style="text-align:left;width:50%;"> Name </th>',
    lines,
    fixed = TRUE
  )
  lines <- sub(
    "<th style=\"text-align:left;\"> Representation </th>",
    '<th style="text-align:center;width:20%;"> Representation </th>',
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

.escape_html_text <- function(x) {
  x <- gsub("&", "&amp;", x, fixed = TRUE)
  x <- gsub("<", "&lt;", x, fixed = TRUE)
  x <- gsub(">", "&gt;", x, fixed = TRUE)
  gsub('"', "&quot;", x, fixed = TRUE)
}
