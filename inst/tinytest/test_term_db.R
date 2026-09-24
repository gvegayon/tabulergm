# ---- Caching mechanism -------------------------------------------------------

# Figures are drawn once and cached; directedness is part of the cache key
edges_fig <- function(directed = FALSE) {
  parse_ergm_formula(y ~ edges, directed = directed)$figure
}
result <- edges_fig()
expect_true(file.exists(result))
expect_equal(edges_fig(), result)
expect_false(identical(edges_fig(directed = TRUE), result))

# Changing the plot function invalidates the cache: rendering the same
# term with a custom plotfun must invoke it and produce a new cache entry
local({
  default_fig <- edges_fig()

  called <- new.env(parent = emptyenv())
  called$n <- 0L
  custom <- function(netobj, layout, vcolor, ecolor, directed, ...) {
    called$n <- called$n + 1L
    invisible(NULL)
  }
  old <- tabulergm_set_plotfun(custom)
  on.exit(tabulergm_set_plotfun(old), add = TRUE)

  custom_fig <- edges_fig()
  expect_equal(called$n, 1L)
  expect_false(identical(default_fig, custom_fig))

  # Re-rendering with the same custom function hits its own cache
  expect_equal(edges_fig(), custom_fig)
  expect_equal(called$n, 1L)

  # Restoring the previous function starts a fresh cache generation,
  # so the figure is redrawn rather than served from the custom entry
  tabulergm_set_plotfun(old)
  restored_fig <- edges_fig()
  expect_false(identical(restored_fig, custom_fig))
  expect_true(file.exists(restored_fig))
})


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

# Drawings reach the plot function with per-edge line types intact (the
# layout-bounding transparent edge is appended last) and with directedness
# on both the argument and the network object, so plot functions such as
# netplot can add arrowheads only for directed networks
local({
  captured <- new.env(parent = emptyenv())
  custom <- function(netobj, layout, vcolor, ecolor, directed,
                     vshape, vrotation, vsize, elinetype, ...) {
    captured$edge_count <- nrow(network::as.edgelist(netobj))
    captured$elinetype <- elinetype
    captured$directed_arg <- directed
    captured$directed_net <- network::is.directed(netobj)
    invisible(NULL)
  }
  old <- tabulergm_set_plotfun(custom)
  on.exit(tabulergm_set_plotfun(old), add = TRUE)

  res <- parse_ergm_formula(y ~ b1nodematch("a"), directed = FALSE)
  expect_true(file.exists(res$figure))
  expect_equal(captured$edge_count, 4L)
  expect_equal(captured$elinetype, c(1, 1, 2, 1))
  expect_false(captured$directed_arg)
  expect_false(captured$directed_net)

  parse_ergm_formula(y ~ edges, directed = TRUE)
  expect_true(captured$directed_arg)
  expect_true(captured$directed_net)
})

# Per-edge attributes are permuted into netplot drawing order (as.edgelist
# sorts by tail/head index, not YAML insertion order). No shipped drawing
# makes a misordering visible, so this one drives the internal renderer
# with a purpose-built spec.
local({
  captured <- new.env(parent = emptyenv())
  custom <- function(netobj, layout, vcolor, ecolor, directed,
                     vshape, vrotation, vsize, elinetype, ...) {
    captured$ecolor <- ecolor
    invisible(NULL)
  }
  old <- tabulergm_set_plotfun(custom)
  on.exit(tabulergm_set_plotfun(old), add = TRUE)

  # Node order is (0, 2, 1): insertion edges are 0->1 = (1,3),
  # 0->2 = (1,2), 2->1 = (2,3); drawing order sorts to (1,2), (1,3),
  # (2,3), so black must move from position 1 to position 2.
  outfile <- tempfile(fileext = ".png")
  tabulergm:::.draw_term_figure(
    list(
      edgelist = "0->1, 0->2, 2->1",
      vcolor = c("black", "gray", "black"),
      ecolor = c("black", "gray", "gray"),
      layout = list(x = c(0, .5, 1), y = c(0, 1, 0))
    ),
    directed = TRUE,
    outfile = outfile
  )

  expect_equal(captured$ecolor, c("gray", "black", "gray", "transparent"))
})


# ---- Terms added in #37 ------------------------------------------------------

# Fitted models: every new term's coefficients get the curated title and a
# rendered figure, and cited terms carry their citation marker
data(sampson, package = "ergm")
data(florentine, package = "ergm")
fit_new <- function(f) suppressMessages(suppressWarnings(
  ergm::ergm(f, estimate = "MPLE")
))
tab_dir <- tabulergm_table(fit_new(
  samplike ~ edges + gwidegree(0.5, fixed = TRUE) +
    gwodegree(0.5, fixed = TRUE) + nodeifactor("group") +
    nodeofactor("group") + istar(2) + ostar(2) +
    dgwesp(0.5, fixed = TRUE, type = "OTP") +
    dgwdsp(0.5, fixed = TRUE, type = "ITP")
), include_title = TRUE)
tab_und <- tabulergm_table(fit_new(
  flomarriage ~ edges + kstar(2) + isolates + degree(1)
), include_title = TRUE)
tab <- rbind(tab_dir, tab_und)
expect_true(all(file.exists(tab$figure)))
expect_equal(unique(tab$title), c(
  "Number of edges",
  "Geometrically weighted in-degree distribution",
  "Geometrically weighted out-degree distribution",
  "Attribute popularity", "Attribute sociality", "In-stars", "Out-stars",
  "Geometrically weighted edgewise shared partners",
  "Geometrically weighted dyadwise shared partners",
  "k-stars", "Isolates", "Degree count"
))
expect_true(all(c("gwidegree (hunter2007; robins2009)",
                  "dgwesp (hunter2007; robins2009)",
                  "kstar (frank1986)") %in% tab$term))

# Formula-only terms: bipartite degree, concurrency, and the directed
# isolates definition (whose drawing contains an isolated node)
res <- parse_ergm_formula(
  y ~ gwb1degree(0.5, fixed = TRUE) + gwb2degree(0.5, fixed = TRUE) +
    concurrent
)
expect_equal(res$citation, c("wang2009, hunter2007", "wang2009, hunter2007", NA))
expect_true(grepl("D^{B_1}_i(y)", res$math[1], fixed = TRUE))
expect_true(grepl("D^{B_2}_i(y)", res$math[2], fixed = TRUE))
expect_true(all(file.exists(res$figure)))
# Citations list the ERGM origin first and at most one theory reference
# after it; preprints are cited by arXiv id
res <- parse_ergm_formula(
  y ~ edges + nodematch("a") + nodemix("a") + b1nodematch("a") + edgecov("d"),
  directed = FALSE
)
expect_equal(res$citation, c(
  "holland1981", "wasserman1996, mcpherson2001", "wasserman1996, morris1991",
  "bomiriya2023", "wasserman1996"
))
md <- as.character(tabulergm_table(
  y ~ nodematch("a") + b1nodematch("a"), directed = FALSE, format = "markdown"
))
expect_true(any(grepl("(wasserman1996; mcpherson2001)", md, fixed = TRUE)))
expect_true(any(grepl("[arXiv:2312.05673](https://arxiv.org/abs/2312.05673)",
  md, fixed = TRUE)))

# `alias:` entries reuse another term's YAML: dgwesp/dgwdsp are gwesp/gwdsp
aliased <- parse_ergm_formula(
  y ~ gwesp(0.5, fixed = TRUE) + dgwesp(0.5, fixed = TRUE) +
    gwdsp(0.5, fixed = TRUE) + dgwdsp(0.5, fixed = TRUE),
  directed = TRUE
)
fields <- c("title", "description", "math", "figure", "citation")
expect_equal(aliased[2, fields], aliased[1, fields], check.attributes = FALSE)
expect_equal(aliased[4, fields], aliased[3, fields], check.attributes = FALSE)

iso <- parse_ergm_formula(y ~ isolates, directed = TRUE)
expect_true(grepl("y_{ji}", iso$math, fixed = TRUE))
expect_true(file.exists(iso$figure))

# Attribute and structural terms resolve on directed networks too
nw_formula_attr <- network::network.initialize(5, directed = TRUE)
f7 <- nw_formula_attr ~ nodematch("a") + absdiff("a") + nodecov("a") +
  nodefactor("a") + nodemix("a") + edgecov("d") + triangle
result7 <- parse_ergm_formula(f7)
for (term in c("nodematch", "absdiff", "nodecov", "nodefactor",
               "nodemix", "edgecov", "triangle")) {
  expect_false(is.na(result7$math[result7$term == term]),
    info = sprintf("formula math found for %s (directed)", term))
  expect_false(is.na(result7$figure[result7$term == term]),
    info = sprintf("formula figure found for %s (directed)", term))
  expect_true(grepl("neq", result7$math[result7$term == term]),
    info = sprintf("directed math (ordered pairs) used for %s", term))
}

# Explicit directedness selects the matching YAML variant
res_directed <- parse_ergm_formula(y ~ edges, directed = TRUE)
expect_true(grepl("neq", res_directed$math))
res_undirected <- parse_ergm_formula(y ~ edges, directed = FALSE)
expect_true(grepl("i<j", res_undirected$math))
# ... and undirected is the default when nothing says otherwise
expect_true(grepl("i<j", parse_ergm_formula(y ~ edges)$math))

# Directedness is inferred from the network on the formula's LHS
nw_formula_dir <- network::network.initialize(5, directed = TRUE)
res_inferred <- parse_ergm_formula(nw_formula_dir ~ edges)
expect_true(grepl("neq", res_inferred$math))

nw_formula_undir <- network::network.initialize(5, directed = FALSE)
res_inferred_u <- parse_ergm_formula(nw_formula_undir ~ edges)
expect_true(grepl("i<j", res_inferred_u$math))


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


# ---- Figure cache is not poisoned by a failed draw --------------------------

# A plot function that errors must not leave a blank PNG behind for the cache
# to serve on every later call in the session.
local({
  previous <- tabulergm_get_plotfun()
  on.exit(tabulergm_set_plotfun(previous), add = TRUE)

  attempts <- 0L
  tabulergm_set_plotfun(function(...) {
    attempts <<- attempts + 1L
    if (attempts == 1L) stop("simulated drawing failure")
    previous(...)
  })

  # First attempt fails and must not cache anything.
  expect_error(parse_ergm_formula(~ edges, directed = FALSE))

  # Second attempt, same cache key, must redraw a figure with actual content.
  figure <- parse_ergm_formula(~ edges, directed = FALSE)$figure
  expect_false(is.na(figure))
  expect_true(file.exists(figure))
  # A redraw actually happened rather than the blank first attempt being
  # served from the cache.
  expect_equal(attempts, 2L)
})
