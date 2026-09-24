# ---- tabulergm_table.formula (no ergm needed) --------------------------------

# tabulergm_table dispatches on formula
f <- y ~ edges + triangle
result <- tabulergm_table(f)
expect_inherits(result, "data.frame")
expect_equal(names(result), c("term", "figure", "math", "description"))
expect_equal(result$term, c("edges", "triangle"))

# formula table excludes coefficient statistics
expect_false("estimate" %in% names(result))
expect_false("se" %in% names(result))
expect_false("pvalue" %in% names(result))

# formula table with attributes
f2 <- y ~ edges + nodematch("gender")
result2 <- tabulergm_table(f2)
expect_equal(result2$term, c("edges", "nodematch"))
# attribute column is NOT in the formula-only output
expect_false("attribute" %in% names(result2))

# tabulergm_table.formula handles one-sided formulas
f3 <- ~ edges + triangle
result3 <- tabulergm_table(f3)
expect_equal(result3$term, c("edges", "triangle"))

# Markdown keeps TeX inequality symbols raw so downstream renderers can parse
# the math expression.
if (requireNamespace("knitr", quietly = TRUE)) {
  md_formula <- tabulergm_table(y ~ edges, format = "markdown")
  md_formula_str <- paste(as.character(md_formula), collapse = "\n")
  expect_true(grepl("\\sum_{i<j} y_{ij}", md_formula_str, fixed = TRUE),
    info = "markdown math keeps raw '<' in TeX")
  expect_false(grepl("\\sum_{i&lt;j} y_{ij}", md_formula_str, fixed = TRUE),
    info = "markdown math does not HTML-escape '<' in TeX")

  md_absdiff <- tabulergm_table(y ~ absdiff("age"), format = "markdown")
  md_absdiff_str <- paste(as.character(md_absdiff), collapse = "\n")
  expect_true(grepl("\\left\\lvert{}x_i - x_j\\right\\rvert{}",
    md_absdiff_str, fixed = TRUE),
    info = "markdown math uses TeX commands for vertical-bar delimiters")
  expect_false(grepl("&#124;", md_absdiff_str, fixed = TRUE),
    info = "markdown math does not contain pipe-table HTML entities")
}

# ---- Drawing-convention notes below rendered tables --------------------------

if (requireNamespace("knitr", quietly = TRUE)) {

  # Structural-only tables have no note
  md <- tabulergm_table(y ~ edges + triangle, format = "markdown")
  md_str <- paste(as.character(md), collapse = "\n")
  expect_false(grepl("Note:", md_str, fixed = TRUE),
    info = "no note for structural-only tables")

  # Attribute terms add the orange note
  md <- tabulergm_table(y ~ edges + nodematch("g"), format = "markdown")
  md_str <- paste(as.character(md), collapse = "\n")
  expect_true(grepl("*Note: Orange nodes", md_str, fixed = TRUE),
    info = "orange note for attribute terms")
  expect_false(grepl("teal", md_str, fixed = TRUE),
    info = "no teal note without mixing terms")
  expect_false(grepl("Square nodes", md_str, fixed = TRUE),
    info = "no bipartite note for one-mode tables")

  # Mixing terms add the orange/teal note, alongside the orange note when an
  # attribute term is also present; terms without a drawing add nothing
  md <- tabulergm_table(y ~ edges + nodemix("g") + twopath,
    format = "markdown")
  md_str <- paste(as.character(md), collapse = "\n")
  expect_true(grepl("Orange and teal nodes", md_str, fixed = TRUE),
    info = "orange/teal note for mixing terms")
  expect_false(grepl("*Note: Orange nodes", md_str, fixed = TRUE),
    info = "no plain orange note for mixing-only tables")
  md <- tabulergm_table(y ~ nodematch("g") + nodemix("g"), format = "markdown")
  md_str <- paste(as.character(md), collapse = "\n")
  expect_true(grepl("Orange nodes", md_str, fixed = TRUE) &&
    grepl("Orange and teal nodes", md_str, fixed = TRUE),
    info = "attribute and mixing terms get both color notes")

  # Bipartite terms add the square/circle note
  md <- tabulergm_table(y ~ edges + b1factor("g"), format = "markdown")
  md_str <- paste(as.character(md), collapse = "\n")
  expect_true(grepl("Square nodes represent nodes in the first mode",
    md_str, fixed = TRUE),
    info = "bipartite note for b1/b2 terms")

  # HTML output wraps the note in a paragraph
  html <- tabulergm_table(y ~ edges + nodematch("g"), format = "html")
  html_str <- paste(as.character(html), collapse = "\n")
  expect_true(grepl('<p class="tabulergm-note">', html_str, fixed = TRUE),
    info = "html note uses a dedicated paragraph")

  # Notes survive in saved markdown and latex output
  out_dir <- tempfile("tabulergm-notes-")
  saved <- tabulergm_save(y ~ edges + nodemix("g"), out_dir)
  md_lines <- readLines(saved$files[["markdown"]])
  expect_true(any(grepl("Orange and teal nodes", md_lines, fixed = TRUE)),
    info = "saved markdown includes the note")
  tex_lines <- readLines(saved$files[["latex"]])
  expect_true(any(grepl("\\emph{Note:", tex_lines, fixed = TRUE)),
    info = "saved latex includes the note")
}


# ---- tabulergm_table.ergm (uses pre-fitted ergm object) ----------------------

if (requireNamespace("network", quietly = TRUE) &&
    requireNamespace("ergm", quietly = TRUE)) {

  library(network)
  library(ergm)

  # Load pre-fitted model (generated by data-raw/fit_ergm_models.R)
  fit <- readRDS(system.file("fits", "fit_edges.rds", package = "tabulergm"))

  # Basic ergm table has default columns
  result <- tabulergm_table(fit)
  expect_inherits(result, "data.frame")
  expect_equal(names(result), c("term", "figure", "estimate", "se", "pvalue"))
  expect_equal(result$term, "edges (holland1981)")
  expect_true(is.numeric(result$estimate))
  expect_true(is.numeric(result$se))
  expect_true(is.character(result$pvalue))

  parsed <- parse_ergm_model(fit)
  result_spec <- attr(result, "tabulergm_spec", exact = TRUE)
  expect_equal(result_spec$data$estimate, parsed$estimate,
    info = "the table specification retains full-precision estimates"
  )
  expect_equal(result_spec$data$se, parsed$se,
    info = "the table specification retains full-precision standard errors"
  )
  expect_equal(result$estimate, round(parsed$estimate, 2),
    info = "default table estimates use two decimal places"
  )
  expect_equal(result$se, round(parsed$se, 2),
    info = "default table standard errors use two decimal places"
  )
  expect_equal(result$pvalue, sprintf("%.2f", parsed$pvalue),
    info = "default table p-values use two decimal places"
  )

  result_zero <- tabulergm_table(fit, digits = 0)
  expect_equal(result_zero$estimate, round(parsed$estimate, 0))
  expect_equal(result_zero$se, round(parsed$se, 0))

  result_full <- tabulergm_table(fit, digits = NULL)
  expect_equal(result_full$estimate, parsed$estimate)
  expect_equal(result_full$se, parsed$se)
  expect_equal(result_full$pvalue, parsed$pvalue)
  expect_null(attr(result_full, "tabulergm_spec", exact = TRUE)$digits)

  # p-values below the display precision show as a bound, never as zero, in
  # the returned table and in the rendered Markdown and LaTeX
  fit_small_p <- readRDS(
    system.file("fits", "fit_nodemix.rds", package = "tabulergm")
  )
  expect_true(parse_ergm_model(fit_small_p)$pvalue[1] < 0.001)
  expect_equal(tabulergm_table(fit_small_p)$pvalue,
    c("<0.01", "0.67", "0.11", "0.15"))
  expect_equal(tabulergm_table(fit_small_p, digits = 3)$pvalue[1], "<0.001")
  md <- paste(tabulergm_table(fit_small_p, format = "markdown"),
    collapse = "\n")
  expect_true(grepl("| &lt;0.01|", md, fixed = TRUE))
  saved_p <- tabulergm_save(fit_small_p, tempfile("tabulergm-pvalue-"),
    format = "latex")
  tex <- paste(readLines(saved_p$files[["latex"]]), collapse = "\n")
  expect_true(grepl("\\textless{}0.01", tex, fixed = TRUE))

  expect_error(tabulergm_table(fit, digits = -1), "digits")
  expect_error(tabulergm_table(fit, digits = 1.5), "digits")

  # Optional columns included when requested
  result_desc <- tabulergm_table(fit, include_description = TRUE)
  expect_true("description" %in% names(result_desc))

  result_math <- tabulergm_table(fit, include_math = TRUE)
  expect_true("math" %in% names(result_math))

  result_attr <- tabulergm_table(fit, include_attribute = TRUE)
  expect_true("attribute" %in% names(result_attr))

  # All optional columns together
  result_all <- tabulergm_table(
    fit,
    include_description = TRUE,
    include_math = TRUE,
    include_attribute = TRUE
  )
  expect_true(all(c("term", "figure", "estimate", "se", "pvalue",
                    "description", "math", "attribute") %in%
                    names(result_all)))

  # coef_name column is NOT included in tabulergm_table output
  expect_false("coef_name" %in% names(result_all))

  # Markdown format (requires knitr)
  if (requireNamespace("knitr", quietly = TRUE)) {
    md <- tabulergm_table(fit, format = "markdown")
    expect_inherits(md, "knitr_kable")

    html <- tabulergm_table(fit, format = "html")
    expect_inherits(html, "knitr_kable")

    # ---- math column uses inline $...$ for markdown ------------------------

    md_math <- tabulergm_table(fit, include_math = TRUE, format = "markdown")
    md_str <- paste(as.character(md_math), collapse = "\n")
    # The math expression should be wrapped in inline $...$ delimiters
    expect_true(grepl("\\$\\\\sum_\\{i<j\\} y_\\{ij\\}\\$", md_str),
      info = "markdown math column contains inline $ delimiters")
    expect_false(grepl("\\$\\$", md_str),
      info = "markdown math column does not use display $$ delimiters")

    # ---- math column is wrapped in $$ for html ----------------------------

    html_math <- tabulergm_table(fit, include_math = TRUE, format = "html")
    html_str <- paste(as.character(html_math), collapse = "\n")
    expect_true(grepl("\\$\\$", html_str),
      info = "html math column contains $$ delimiters")

    # Inequality symbols should be HTML-escaped so MathJax can parse reliably
    html_formula <- tabulergm_table(
      network ~ b2nodematch("group"),
      format = "html"
    )
    html_formula_str <- paste(as.character(html_formula), collapse = "\n")
    expect_true(grepl("&lt;", html_formula_str),
      info = "html math escapes '<' as &lt; in TeX")

    # ---- figure column uses markdown image syntax --------------------------

    md_fig <- tabulergm_table(fit, format = "markdown")
    md_fig_str <- paste(as.character(md_fig), collapse = "\n")
    # The figure cell should contain markdown image syntax (or be empty)
    has_figure <- !is.na(tabulergm_table(fit)[["figure"]])
    if (any(has_figure)) {
      expect_true(grepl("!\\[]\\(", md_fig_str),
        info = "markdown figure column uses markdown image syntax")
      expect_true(grepl("{width=80px}",
        md_fig_str,
        fixed = TRUE
      ), info = "markdown figure syntax sets width")
    }

    # ---- figure column uses <img> tags for html ---------------------------

    html_fig_str <- paste(as.character(html), collapse = "\n")
    if (any(has_figure)) {
      expect_true(grepl("<img", html_fig_str),
        info = "html figure column uses <img> tags")
      expect_true(grepl("style=\"width:80px;max-width:100%;\"",
        html_fig_str,
        fixed = TRUE
      ), info = "html figure style sets width and max-width")
      expect_false(grepl("height:80px", html_fig_str, fixed = TRUE),
        info = "html figure style does not set a fixed height")
      expect_true(grepl("data:image/[^;]+;base64,", html_fig_str),
        info = "html figure column inlines image data as data URI")
    }

    # ---- markdown figures can be copied to a user folder ------------------

    # A relative figures_dir resolves against the working directory, and the
    # copied files are named after the bare term, even when the term column
    # carries a citation marker (no description column shown)
    local({
      out_dir <- tempfile("tabulergm-figures-")
      dir.create(out_dir)
      old_wd <- setwd(out_dir)
      on.exit(setwd(old_wd), add = TRUE)

      md <- paste(as.character(tabulergm_table(
        ~ edges + triangle, directed = FALSE, include_description = FALSE,
        format = "markdown", figures_dir = "assets"
      )), collapse = "\n")
      expect_true(grepl("triangle (frank1986)", md, fixed = TRUE))
      expect_equal(sort(list.files("assets")), c("edges.png", "triangle.png"),
        info = "manual figures_dir copies markdown figures")
      expect_true(grepl("![](assets/edges.png){width=80px}", md, fixed = TRUE),
        info = "manual figures_dir rewrites markdown figure path")
    })

    # An invalid figures_dir is reported when the table is built (#35), not
    # deferred until something renders Markdown.
    expect_error(
      tabulergm_table(~ edges, directed = FALSE, figures_dir = 42),
      "figures_dir"
    )

    # ---- markdown figures use the active knitr figure path ----------------

    local({
      old_knit <- knitr::opts_knit$get()
      old_current <- knitr::opts_current$get()
      on.exit(knitr::opts_knit$set(old_knit), add = TRUE)
      on.exit(knitr::opts_current$set(old_current), add = TRUE)

      out_dir <- tempfile("tabulergm-knitr-figures-")
      dir.create(out_dir)

      knitr::opts_knit$set(
        output.dir = out_dir,
        rmarkdown.pandoc.to = "gfm"
      )

      # A fig.path prefix is prepended to the file name ...
      knitr::opts_current$set(fig.path = "man/figures/README-")
      md <- paste(as.character(
        tabulergm_table(~ edges, directed = FALSE, format = "markdown")
      ), collapse = "\n")
      expect_true(file.exists(file.path(out_dir, "man", "figures",
        "README-edges.png"
      )), info = "knitr fig.path prefix receives markdown figures")
      expect_true(grepl("![](man/figures/README-edges.png){width=80px}",
        md, fixed = TRUE
      ), info = "knitr fig.path prefix rewrites markdown figure path")

      # ... and a fig.path directory holds it, without a doubled slash
      knitr::opts_current$set(fig.path = "README_files/figure-gfm/")
      md <- paste(as.character(
        tabulergm_table(~ triangle, directed = FALSE, format = "markdown")
      ), collapse = "\n")
      expect_true(file.exists(file.path(out_dir, "README_files",
        "figure-gfm", "triangle.png"
      )), info = "knitr fig.path directory receives markdown figures")
      expect_true(grepl("![](README_files/figure-gfm/triangle.png){width=80px}",
        md, fixed = TRUE
      ), info = "knitr fig.path directory rewrites markdown figure path")
      expect_false(grepl("figure-gfm//", md, fixed = TRUE),
        info = "knitr fig.path directory does not emit a double slash")
    })

    # ---- tabulergm_view dispatches without error --------------------------

    # Only test that the function returns a path; skip browser opening
    tmp <- tabulergm_view(fit)
    expect_true(file.exists(tmp),
      info = "tabulergm_view creates a temporary HTML file")
    expect_true(grepl("\\.html$", tmp),
      info = "tabulergm_view returns a .html file path")

    # The HTML file contains MathJax script tag
    html_content <- paste(readLines(tmp), collapse = "\n")
    expect_true(grepl("mathjax", tolower(html_content)),
      info = "tabulergm_view HTML includes MathJax")
    expect_true(grepl("data:image/[^;]+;base64,", html_content),
      info = "tabulergm_view HTML inlines figures as data URIs")

    # tabulergm_view also works on a formula
    tmp_f <- tabulergm_view(network ~ edges)
    expect_true(file.exists(tmp_f),
      info = "tabulergm_view.formula creates a temporary HTML file")
  }
}
