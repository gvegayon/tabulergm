# ---- Edgelist parsing --------------------------------------------------------

# .parse_plot_edgelist parses a simple edge
edges <- tabulergm:::.parse_plot_edgelist("0->1")
expect_equal(nrow(edges), 1L)
expect_equal(unname(edges[1L, "from"]), "0")
expect_equal(unname(edges[1L, "to"]), "1")

# .parse_plot_edgelist parses a 3-cycle
edges <- tabulergm:::.parse_plot_edgelist("0->1->2->0")
expect_equal(nrow(edges), 3L)
expect_equal(edges[, "from"], c("0", "1", "2"))
expect_equal(edges[, "to"],   c("1", "2", "0"))

# .parse_plot_edgelist errors on a single node
expect_error(tabulergm:::.parse_plot_edgelist("0"))

# .parse_plot_edgelist trims whitespace
edges <- tabulergm:::.parse_plot_edgelist(" 0 -> 1 -> 2 ")
expect_equal(nrow(edges), 2L)
expect_equal(edges[, "from"], c("0", "1"))
expect_equal(edges[, "to"],   c("1", "2"))

# .parse_plot_edgelist supports comma-separated segments
edges <- tabulergm:::.parse_plot_edgelist("0->1, 2->1")
expect_equal(nrow(edges), 2L)
expect_equal(edges[, "from"], c("0", "2"))
expect_equal(edges[, "to"],   c("1", "1"))

# .parse_plot_edgelist handles mixed chains and commas
edges <- tabulergm:::.parse_plot_edgelist("0->1->2, 3->4")
expect_equal(nrow(edges), 3L)
expect_equal(edges[, "from"], c("0", "1", "3"))
expect_equal(edges[, "to"],   c("1", "2", "4"))


# ---- YAML file lookup --------------------------------------------------------

# Test known terms via for-loop
yml_test_cases <- list(
  list(term = "edges",    directed = FALSE, pattern = "edges\\.undirected\\.yml$"),
  list(term = "edges",    directed = TRUE,  pattern = "edges\\.directed\\.yml$"),
  list(term = "edges",    directed = NULL,  pattern = "edges\\.undirected\\.yml$"),
  list(term = "mutual",   directed = TRUE,  pattern = "mutual\\.directed\\.yml$"),
  list(term = "mutual",   directed = NULL,  pattern = "mutual\\.directed\\.yml$"),
  list(term = "triangle", directed = FALSE, pattern = "triangle\\.undirected\\.yml$")
)

for (tc in yml_test_cases) {
  path <- tabulergm:::.find_term_yml(tc$term, directed = tc$directed)
  expect_true(!is.null(path),
    info = sprintf("YAML found for %s (directed=%s)", tc$term,
                   deparse(tc$directed)))
  expect_true(grepl(tc$pattern, path),
    info = sprintf("Pattern matches for %s (directed=%s)", tc$term,
                   deparse(tc$directed)))
}

# .find_term_yml returns NULL for missing terms
path <- tabulergm:::.find_term_yml("nonexistent_term_xyz", directed = FALSE)
expect_null(path)

# .find_term_yml strips offset() wrapper
path <- tabulergm:::.find_term_yml("offset(edges)", directed = FALSE)
expect_true(!is.null(path))
expect_true(grepl("edges\\.undirected\\.yml$", path))


# ---- YAML data reading ------------------------------------------------------

# .get_term_yml_data returns math from YAML
data <- tabulergm:::.get_term_yml_data("edges", directed = FALSE)
expect_false(is.na(data$math))
expect_true(grepl("sum", data$math))

# .get_term_yml_data returns NA for unknown terms
data <- tabulergm:::.get_term_yml_data("nonexistent_term_xyz", directed = FALSE)
expect_true(is.na(data$math))
expect_true(is.na(data$figure))

# .get_term_yml_data reads nodematch math
data <- tabulergm:::.get_term_yml_data("nodematch", directed = FALSE)
expect_false(is.na(data$math))
expect_true(grepl("x_i = x_j", data$math))

# .get_term_yml_data reads triangle math
data <- tabulergm:::.get_term_yml_data("triangle", directed = FALSE)
expect_false(is.na(data$math))


# ---- Caching mechanism -------------------------------------------------------

# .get_cached_figure draws and caches a figure
yml_path <- tabulergm:::.find_term_yml("edges", directed = FALSE)
yml_data <- yaml::read_yaml(yml_path, handlers = list(
  "bool#yes" = function(x) x,
  "bool#no"  = function(x) x
))
result <- tabulergm:::.get_cached_figure(
  yml_path, yml_data$plot, directed = FALSE
)
expect_true(!is.na(result))
expect_true(file.exists(result))

# Calling again returns the cached path (same file)
result2 <- tabulergm:::.get_cached_figure(
  yml_path, yml_data$plot, directed = FALSE
)
expect_equal(result, result2)


# ---- Plotfun API -------------------------------------------------------------

# tabulergm_get_plotfun returns the default by default
pfun <- tabulergm_get_plotfun()
expect_true(is.function(pfun))
expect_identical(pfun, tabulergm_default_plotfun)

# tabulergm_set_plotfun sets a custom function
custom <- function(netobj, layout, vcolor, ecolor, directed, ...) NULL
old <- tabulergm_set_plotfun(custom)
expect_identical(tabulergm_get_plotfun(), custom)

# tabulergm_set_plotfun returns the previous function
expect_identical(old, tabulergm_default_plotfun)

# Restore default
tabulergm_set_plotfun(tabulergm_default_plotfun)
expect_identical(tabulergm_get_plotfun(), tabulergm_default_plotfun)

# tabulergm_set_plotfun errors on non-function
expect_error(tabulergm_set_plotfun("not a function"))


# ---- Integration with parse_ergm_formula -------------------------------------

# parse_ergm_formula populates math from YAML for known terms
f <- y ~ edges + triangle
result <- parse_ergm_formula(f)
# edges has a YAML definition, so math should not be NA
expect_false(is.na(result$math[result$term == "edges"]))
# triangle has a YAML definition
expect_false(is.na(result$math[result$term == "triangle"]))

# nodematch has YAML data
f2 <- y ~ edges + nodematch("gender")
result2 <- parse_ergm_formula(f2)
expect_false(is.na(result2$math[result2$term == "nodematch"]))

# Unknown terms still have NA math
f3 <- y ~ edges + gwesp(0.5, fixed = TRUE)
result3 <- parse_ergm_formula(f3)
expect_false(is.na(result3$math[result3$term == "edges"]))
# gwesp has no YAML file, so math stays NA
expect_true(is.na(result3$math[result3$term == "gwesp"]))


# ---- Integration with parse_ergm_model (requires ergm) -----------------------

if (requireNamespace("ergm", quietly = TRUE)) {

  library(network)
  library(ergm)

  # parse_ergm_model populates math from YAML for known terms
  nw <- network(10, directed = FALSE, density = 0.3)
  suppressWarnings(fit <- ergm(nw ~ edges))
  result <- parse_ergm_model(fit)
  # edges has a YAML definition with undirected math
  expect_false(is.na(result$math[result$term == "edges"]))
  expect_true(grepl("i<j", result$math[result$term == "edges"]))

  # Directed network uses directed YAML
  nw_d <- network(10, directed = TRUE, density = 0.2)
  suppressWarnings(fit_d <- ergm(nw_d ~ edges))
  result_d <- parse_ergm_model(fit_d)
  expect_false(is.na(result_d$math[result_d$term == "edges"]))
  expect_true(grepl("neq", result_d$math[result_d$term == "edges"]))
}
