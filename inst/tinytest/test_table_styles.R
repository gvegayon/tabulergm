# ---- Composable table styles -------------------------------------------------

plain <- tabulergm_table(~ edges + triangle, directed = FALSE)
expect_inherits(plain, "tabulergm_table")
expect_inherits(plain, "data.frame")

# Formula tables now honor the documented description switch.
without_description <- tabulergm_table(
  ~ edges + triangle,
  directed = FALSE,
  include_description = FALSE
)
expect_false("description" %in% names(without_description))

compact <- with_style_name_over_formula(without_description)
expect_equal(names(compact), c("Name", "Representation"))
expect_true(grepl("\n", compact$Name[[1L]], fixed = TRUE),
  info = "the data-frame Name cell has a title/formula line break")
expect_true(grepl("\\sum_{i<j} y_{ij}", compact$Name[[1L]], fixed = TRUE),
  info = "the data-frame Name cell retains raw TeX")
expect_equal(compact$Representation, without_description$figure)
expect_true(grepl("(frank1986)", compact$Name[[2L]], fixed = TRUE),
  info = "citations move to Name when description is absent")

# Styles are idempotent and the plain style restores the selected source data.
compact_again <- with_style_name_over_formula(compact)
expect_equal(compact_again, compact)
restored <- with_style_plain(compact)
expect_equal(names(restored), names(without_description))
expect_equal(restored$term, without_description$term)
expect_equal(restored$math, without_description$math)

# When retained, descriptions remain the citation target.
compact_with_description <- with_style_name_over_formula(plain)
expect_true("description" %in% names(compact_with_description))
expect_true(grepl("(frank1986)", compact_with_description$description[[2L]],
  fixed = TRUE
))
expect_false(grepl("(frank1986)", compact_with_description$Name[[2L]],
  fixed = TRUE
))

expect_error(
  with_style_name_over_formula(data.frame(term = "edges")),
  "tabulergm_table"
)

if (requireNamespace("knitr", quietly = TRUE)) {
  # An HTML table can be restyled after it has already been rendered.
  html <- tabulergm_table(
    ~ edges + triangle,
    directed = FALSE,
    include_description = FALSE,
    format = "html"
  )
  compact_html <- with_style_name_over_formula(html)
  html_text <- paste(as.character(compact_html), collapse = "\n")
  expect_inherits(compact_html, "tabulergm_kable")
  expect_inherits(compact_html, "knitr_kable")
  expect_true(grepl("tabulergm-style-name-over-formula", html_text,
    fixed = TRUE
  ))
  expect_true(grepl('width:40%;max-width:100%;', html_text, fixed = TRUE))
  expect_true(grepl("data:image/[^;]+;base64,", html_text))
  expect_true(grepl("i&lt;j", html_text, fixed = TRUE),
    info = "compact HTML preserves safely escaped TeX"
  )
  expect_true(grepl("frank1986", html_text, fixed = TRUE))

  # Styled Markdown intentionally uses raw HTML for its multi-line cells.
  figures_dir <- tempfile("tabulergm-style-markdown-")
  markdown <- tabulergm_table(
    ~ edges,
    directed = FALSE,
    include_description = FALSE,
    format = "markdown",
    figures_dir = figures_dir
  )
  compact_markdown <- with_style_name_over_formula(markdown)
  markdown_text <- paste(as.character(compact_markdown), collapse = "\n")
  expect_equal(attr(compact_markdown, "format"), "html")
  expect_true(grepl("<table", markdown_text, fixed = TRUE))
  expect_true(grepl("<img src=", markdown_text, fixed = TRUE))
  expect_true(grepl("i\\lt{}j", markdown_text, fixed = TRUE),
    info = "raw-HTML Markdown uses TeX-safe inequality operators"
  )
  expect_false(grepl("&lt;", markdown_text, fixed = TRUE),
    info = "GitHub must not receive an HTML entity inside TeX"
  )
  expect_true(file.exists(file.path(figures_dir, "edges.png")))

  # Styled data frames and kables can be exported and viewed through pipes.
  out_dir <- tempfile("tabulergm-style-save-")
  saved <- tabulergm_save(compact, out_dir)
  expect_equal(names(saved$table), c("Name", "Representation"))
  expect_true(all(file.exists(saved$files)))
  saved_md <- paste(readLines(saved$files[["markdown"]]), collapse = "\n")
  expect_true(grepl("tabulergm-style-name-over-formula", saved_md,
    fixed = TRUE
  ))
  saved_tex <- paste(readLines(saved$files[["latex"]]), collapse = "\n")
  expect_true(grepl("\\usepackage{array,booktabs,graphicx}", saved_tex,
    fixed = TRUE
  ))
  expect_true(grepl("\\begin{minipage}{\\linewidth}", saved_tex,
    fixed = TRUE
  ))
  expect_true(grepl("\\includegraphics[width=.4\\linewidth]", saved_tex,
    fixed = TRUE
  ))
  expect_true(grepl("\\toprule", saved_tex, fixed = TRUE))

  view_path <- tabulergm_view(compact)
  expect_true(file.exists(view_path))
  view_html <- paste(readLines(view_path), collapse = "\n")
  expect_true(grepl("mathjax", tolower(view_html)))
  expect_true(grepl("tabulergm-style-name-over-formula", view_html,
    fixed = TRUE
  ))
}

if (requireNamespace("network", quietly = TRUE) &&
    requireNamespace("ergm", quietly = TRUE)) {
  fit <- readRDS(system.file("fits", "fit_edges.rds", package = "tabulergm"))
  fitted <- tabulergm_table(
    fit,
    include_title = TRUE,
    include_math = TRUE,
    include_description = TRUE,
    include_attribute = TRUE
  )
  fitted_compact <- with_style_name_over_formula(fitted)
  expect_equal(
    names(fitted_compact),
    c("Name", "Representation", "estimate", "se", "pvalue",
      "description", "attribute")
  )
  expect_true(is.numeric(fitted_compact$estimate))
  expect_true("description" %in% names(fitted_compact))
}
