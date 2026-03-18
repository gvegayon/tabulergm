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


# ---- YAML file lookup --------------------------------------------------------

# .find_term_yml finds edges.undirected.yml
path <- tabulergm:::.find_term_yml("edges", directed = FALSE)
expect_true(!is.null(path))
expect_true(grepl("edges\\.undirected\\.yml$", path))

# .find_term_yml finds edges.directed.yml
path <- tabulergm:::.find_term_yml("edges", directed = TRUE)
expect_true(!is.null(path))
expect_true(grepl("edges\\.directed\\.yml$", path))

# .find_term_yml returns NULL for missing terms
path <- tabulergm:::.find_term_yml("nonexistent_term_xyz", directed = FALSE)
expect_null(path)

# .find_term_yml with directed = NULL finds undirected first
path <- tabulergm:::.find_term_yml("edges", directed = NULL)
expect_true(!is.null(path))
expect_true(grepl("edges\\.undirected\\.yml$", path))

# .find_term_yml finds mutual.directed.yml
path <- tabulergm:::.find_term_yml("mutual", directed = TRUE)
expect_true(!is.null(path))
expect_true(grepl("mutual\\.directed\\.yml$", path))

# .find_term_yml with directed = NULL finds directed if no undirected
path <- tabulergm:::.find_term_yml("mutual", directed = NULL)
expect_true(!is.null(path))
expect_true(grepl("mutual\\.directed\\.yml$", path))

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

# .get_cached_figure returns NA when netplot is not available (or uses cache)
yml_path <- tabulergm:::.find_term_yml("edges", directed = FALSE)
yml_data <- yaml::read_yaml(yml_path)
result <- tabulergm:::.get_cached_figure(
  yml_path, yml_data$plot, directed = FALSE, engine = "netplot"
)
# If netplot is not installed, result should be NA
if (!requireNamespace("netplot", quietly = TRUE)) {
  expect_true(is.na(result))
}


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


# ---- Integration with parse_ergm_model (requires ergm and network) -----------

if (requireNamespace("network", quietly = TRUE) &&
    requireNamespace("ergm", quietly = TRUE)) {

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
