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
  list(term = "triangle", directed = FALSE, pattern = "triangle\\.undirected\\.yml$"),
  list(term = "gwb1dsp", directed = FALSE, pattern = "gwb1dsp\\.undirected\\.yml$"),
  list(term = "gwb2dsp", directed = FALSE, pattern = "gwb2dsp\\.undirected\\.yml$"),
  list(term = "b1factor", directed = FALSE, pattern = "b1factor\\.undirected\\.yml$"),
  list(term = "b2factor", directed = FALSE, pattern = "b2factor\\.undirected\\.yml$"),
  list(term = "b1nodematch", directed = FALSE, pattern = "b1nodematch\\.undirected\\.yml$"),
  list(term = "b2nodematch", directed = FALSE, pattern = "b2nodematch\\.undirected\\.yml$"),
  list(term = "b1starmix", directed = FALSE, pattern = "b1starmix\\.undirected\\.yml$"),
  list(term = "b2starmix", directed = FALSE, pattern = "b2starmix\\.undirected\\.yml$"),
  list(term = "gwesp", directed = FALSE, pattern = "gwesp\\.undirected\\.yml$"),
  list(term = "gwesp", directed = TRUE,  pattern = "gwesp\\.directed\\.yml$"),
  list(term = "gwdsp", directed = FALSE, pattern = "gwdsp\\.undirected\\.yml$"),
  list(term = "gwdsp", directed = TRUE,  pattern = "gwdsp\\.directed\\.yml$"),
  list(term = "gwdegree", directed = FALSE, pattern = "gwdegree\\.undirected\\.yml$"),
  list(term = "nodefactor", directed = FALSE, pattern = "nodefactor\\.undirected\\.yml$"),
  list(term = "nodecov", directed = FALSE, pattern = "nodecov\\.undirected\\.yml$"),
  list(term = "absdiff", directed = FALSE, pattern = "absdiff\\.undirected\\.yml$"),
  list(term = "nodemix", directed = FALSE, pattern = "nodemix\\.undirected\\.yml$"),
  list(term = "edgecov", directed = FALSE, pattern = "edgecov\\.undirected\\.yml$"),
  list(term = "transitiveties", directed = TRUE, pattern = "transitiveties\\.directed\\.yml$"),
  list(term = "cyclicalties", directed = TRUE, pattern = "cyclicalties\\.directed\\.yml$"),
  list(term = "nodeicov", directed = TRUE, pattern = "nodeicov\\.directed\\.yml$"),
  list(term = "nodeocov", directed = TRUE, pattern = "nodeocov\\.directed\\.yml$")
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

# .get_term_yml_data reads bipartite term math
for (term in c("gwb1dsp", "gwb2dsp", "b1factor", "b2factor",
               "b1nodematch", "b2nodematch", "b1starmix", "b2starmix")) {
  data <- tabulergm:::.get_term_yml_data(term, directed = FALSE)
  expect_false(is.na(data$math),
    info = sprintf("math found for %s", term))
  expect_false(is.na(data$figure),
    info = sprintf("figure found for %s", term))
}

# .get_term_yml_data reads undirected key term math and figures
for (term in c("gwesp", "gwdsp", "gwdegree", "nodefactor", "nodecov",
               "absdiff", "nodemix", "edgecov")) {
  data <- tabulergm:::.get_term_yml_data(term, directed = FALSE)
  expect_false(is.na(data$math),
    info = sprintf("math found for %s (undirected)", term))
  expect_false(is.na(data$figure),
    info = sprintf("figure found for %s (undirected)", term))
}

# .get_term_yml_data reads directed key term math and figures
for (term in c("gwesp", "gwdsp", "transitiveties", "cyclicalties",
               "nodeicov", "nodeocov")) {
  data <- tabulergm:::.get_term_yml_data(term, directed = TRUE)
  expect_false(is.na(data$math),
    info = sprintf("math found for %s (directed)", term))
  expect_false(is.na(data$figure),
    info = sprintf("figure found for %s (directed)", term))
}


# ---- Caching mechanism -------------------------------------------------------

# .get_cached_figure draws and caches a figure
yml_path <- tabulergm:::.find_term_yml("edges", directed = FALSE)
# Use bool handlers to prevent YAML 1.1 coercion of 'y' key to TRUE
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

# .draw_term_figure preserves edge-specific line types when adding layout bounds
local({
  captured <- new.env(parent = emptyenv())
  custom <- function(netobj, layout, vcolor, ecolor, directed,
                     vshape, vrotation, vsize, elinetype, ...) {
    captured$edge_count <- nrow(network::as.edgelist(netobj))
    captured$elinetype <- elinetype
    invisible(NULL)
  }
  old <- tabulergm_set_plotfun(custom)
  on.exit(tabulergm_set_plotfun(old), add = TRUE)

  outfile <- tempfile(fileext = ".png")
  result <- tabulergm:::.draw_term_figure(
    list(
      edgelist = "0->2, 0->1, 0->3",
      vcolor = c("orange", "orange", "orange", "gray"),
      ecolor = "black",
      vshape = c("square", "circle", "circle", "circle"),
      vsize = c(1.0, 0.5, 0.5, 0.5),
      elinetype = c(1, 1, 2),
      layout = list(x = c(0, 1, 1, 1), y = c(0, 0.5, 0, -0.5))
    ),
    directed = FALSE,
    outfile = outfile
  )

  expect_true(file.exists(result))
  expect_equal(captured$edge_count, 4L)
  expect_equal(captured$elinetype, c(1, 1, 2, 1))
})


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

# gwesp now has a YAML definition
f3 <- y ~ edges + gwesp(0.5, fixed = TRUE)
result3 <- parse_ergm_formula(f3)
expect_false(is.na(result3$math[result3$term == "edges"]))
expect_false(is.na(result3$math[result3$term == "gwesp"]))

# Unknown terms still have NA math
f3b <- y ~ edges + kstar(2)
result3b <- parse_ergm_formula(f3b)
expect_false(is.na(result3b$math[result3b$term == "edges"]))
# kstar has no YAML file, so math stays NA
expect_true(is.na(result3b$math[result3b$term == "kstar"]))

# Bipartite terms have YAML data
f4 <- y ~ gwb1dsp(0.5, fixed = TRUE) + b1factor("type") + b2nodematch("group") +
  b1starmix(2, "type") + b2starmix(2, "type")
result4 <- parse_ergm_formula(f4)
expect_false(is.na(result4$math[result4$term == "gwb1dsp"]))
expect_false(is.na(result4$math[result4$term == "b1factor"]))
expect_false(is.na(result4$math[result4$term == "b2nodematch"]))
expect_false(is.na(result4$math[result4$term == "b1starmix"]))
expect_false(is.na(result4$math[result4$term == "b2starmix"]))

# Key covariate and structural terms have YAML data
f5 <- y ~ gwdsp(0.5, fixed = TRUE) + gwdegree(0.5, fixed = TRUE) +
  nodefactor("race") + nodecov("age") + absdiff("age") +
  nodemix("race") + edgecov("dist")
result5 <- parse_ergm_formula(f5)
for (term in c("gwdsp", "gwdegree", "nodefactor", "nodecov", "absdiff",
               "nodemix", "edgecov")) {
  expect_false(is.na(result5$math[result5$term == term]),
    info = sprintf("formula math found for %s", term))
}

# Directed-only terms have YAML data
f6 <- y ~ transitiveties + cyclicalties + nodeicov("age") + nodeocov("age")
result6 <- parse_ergm_formula(f6)
for (term in c("transitiveties", "cyclicalties", "nodeicov", "nodeocov")) {
  expect_false(is.na(result6$math[result6$term == term]),
    info = sprintf("formula math found for %s", term))
}


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
