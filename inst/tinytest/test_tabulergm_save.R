# ---- tabulergm_save ---------------------------------------------------------

if (requireNamespace("knitr", quietly = TRUE)) {

  # Formula objects can be exported to Markdown and LaTeX with copied figures.
  out_dir <- tempfile("tabulergm-save-")
  saved <- tabulergm_save(
    y ~ edges + triangle,
    out_dir,
    format = c("markdown", "latex")
  )

  expect_true(dir.exists(out_dir))
  expect_equal(names(saved$files), c("markdown", "latex"))
  expect_true(all(file.exists(saved$files)))
  expect_true(all(file.exists(saved$figures)))
  expect_equal(saved$table$figure, c("figures/edges.png", "figures/triangle.png"))

  md <- paste(readLines(saved$files[["markdown"]]), collapse = "\n")
  expect_true(grepl("![](figures/edges.png){width=80px}", md, fixed = TRUE),
    info = "markdown export references copied figures")
  expect_true(grepl("$\\sum_{i<j} y_{ij}$", md, fixed = TRUE),
    info = "markdown export includes inline math")

  tex <- paste(readLines(saved$files[["latex"]]), collapse = "\n")
  expect_true(grepl("\\includegraphics[width=0.7in]{figures/edges.png}",
    tex, fixed = TRUE),
    info = "latex export references copied figures")
  expect_true(grepl("$\\sum_{i<j} y_{ij}$", tex, fixed = TRUE),
    info = "latex export wraps math in inline math delimiters")
  expect_equal(
    tabulergm:::.escape_latex_text("a_b\\c"),
    "a\\_b\\textbackslash{}c"
  )

  # A data frame returned by tabulergm_table can also be exported.
  src <- tempfile(fileext = ".png")
  writeLines("fake image content", src)
  df <- data.frame(
    term = "Custom Term",
    figure = src,
    math = "\\sum_i x_i",
    stringsAsFactors = FALSE
  )

  out_dir_df <- tempfile("tabulergm-save-df-")
  saved_df <- tabulergm_save(df, out_dir_df, format = "markdown")
  expect_equal(saved_df$table$figure, "figures/custom-term.png")
  expect_true(file.exists(file.path(out_dir_df, "figures", "custom-term.png")))
  expect_equal(names(saved_df$files), "markdown")

  # Images can be written directly into the target directory when requested.
  out_dir_flat <- tempfile("tabulergm-save-flat-")
  saved_flat <- tabulergm_save(
    df,
    out_dir_flat,
    format = "markdown",
    images_dir = "."
  )
  expect_equal(saved_flat$table$figure, "custom-term.png")
  expect_true(file.exists(file.path(out_dir_flat, "custom-term.png")))

  # Existing files are protected when overwrite = FALSE.
  expect_error(
    tabulergm_save(y ~ edges, out_dir, overwrite = FALSE),
    "overwrite",
    info = "overwrite = FALSE protects existing export files"
  )
}
