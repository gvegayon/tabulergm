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

# ---- tabulergm_table.ergm (requires ergm and network) -----------------------

if (requireNamespace("network", quietly = TRUE) &&
    requireNamespace("ergm", quietly = TRUE)) {

  library(network)
  library(ergm)

  # Basic ergm table has default columns
  nw <- network(10, directed = FALSE, density = 0.3)
  suppressWarnings(fit <- ergm(nw ~ edges))
  result <- tabulergm_table(fit)
  expect_inherits(result, "data.frame")
  expect_equal(names(result), c("term", "figure", "estimate", "se", "pvalue"))
  expect_equal(result$term, "edges")
  expect_true(is.numeric(result$estimate))
  expect_true(is.numeric(result$se))
  expect_true(is.numeric(result$pvalue))

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
    expect_inherits(md, "character")

    html <- tabulergm_table(fit, format = "html")
    expect_inherits(html, "character")
  }
}
