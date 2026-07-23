# ---- Formula parsing (no ergm needed) ----------------------------------------

# parse_ergm_formula works with a simple formula
f <- y ~ edges + triangle
result <- parse_ergm_formula(f)
expect_inherits(result, "data.frame")
expect_equal(result$term, c("edges", "triangle"))
expect_true(all(is.na(result$attribute)))
expect_true(all(is.na(result$estimate)))
expect_true(all(is.na(result$se)))
expect_true(all(is.na(result$pvalue)))
expect_true(all(c("description", "math", "figure") %in% names(result)))

# parse_ergm_formula extracts attributes from terms
f <- y ~ edges + nodematch("gender") + nodefactor("race")
result <- parse_ergm_formula(f)
expect_equal(result$term, c("edges", "nodematch", "nodefactor"))
expect_equal(result$attribute, c(NA_character_, "gender", "race"))

# parse_ergm_formula works with a single term
f <- y ~ edges
result <- parse_ergm_formula(f)
expect_equal(nrow(result), 1L)
expect_equal(result$term, "edges")
expect_true(is.na(result$attribute))

# parse_ergm_formula handles one-sided formulas
f <- ~ edges + triangle
result <- parse_ergm_formula(f)
expect_equal(result$term, c("edges", "triangle"))

# parse_ergm_formula handles offset() terms
f <- y ~ edges + offset(nodematch("gender"))
result <- parse_ergm_formula(f)
expect_equal(result$term, c("edges", "offset(nodematch)"))
expect_equal(result$attribute, c(NA_character_, "gender"))

# parse_ergm_formula handles curved terms
f <- y ~ edges + gwesp(0.5, fixed = TRUE)
result <- parse_ergm_formula(f)
expect_equal(result$term, c("edges", "gwesp"))
expect_true(is.na(result$attribute[2]))

# parse_ergm_formula handles multiple attributes (e.g., mixing)
f <- y ~ edges + nodemix("race", "gender")
result <- parse_ergm_formula(f)
expect_equal(result$term, c("edges", "nodemix"))
expect_equal(result$attribute[2], "race, gender")

# parse_ergm_formula keeps attributes of repeated terms separate
f <- y ~ edges + nodecov("wealth") + nodecov("priorates")
result <- parse_ergm_formula(f)
expect_equal(result$term, c("edges", "nodecov", "nodecov"))
expect_equal(result$attribute, c(NA_character_, "wealth", "priorates"))

# parse_ergm_formula errors on non-formula input
expect_error(parse_ergm_formula("not a formula"))
expect_error(parse_ergm_formula(42))

# parse_ergm_formula validates the 'directed' argument
expect_error(parse_ergm_formula(y ~ edges, directed = NA))
expect_error(parse_ergm_formula(y ~ edges, directed = c(TRUE, FALSE)))
expect_error(parse_ergm_formula(y ~ edges, directed = "yes"))

# .infer_formula_directedness reads directedness from the LHS network
nw_directed   <- network::network.initialize(5, directed = TRUE)
nw_undirected <- network::network.initialize(5, directed = FALSE)
expect_true(tabulergm:::.infer_formula_directedness(nw_directed ~ edges))
expect_false(tabulergm:::.infer_formula_directedness(nw_undirected ~ edges))

# ... and returns NULL for one-sided formulas or non-network LHS
expect_null(tabulergm:::.infer_formula_directedness(~ edges))
expect_null(tabulergm:::.infer_formula_directedness(undefined_object ~ edges))
expect_null(tabulergm:::.infer_formula_directedness(letters ~ edges))

# parse_ergm_formula handles bipartite terms
f <- y ~ edges + b1star(2) + gwb1dsp(0.5, fixed = TRUE) +
  gwb2dsp(0.5, fixed = TRUE) + b1factor("type") + b2factor("group") +
  b1nodematch("type") + b2nodematch("group")
result <- parse_ergm_formula(f)
expect_equal(
  result$term,
  c("edges", "b1star", "gwb1dsp", "gwb2dsp",
    "b1factor", "b2factor", "b1nodematch", "b2nodematch")
)
expect_true(is.na(result$attribute[1]))
expect_true(is.na(result$attribute[2]))
expect_true(is.na(result$attribute[3]))
expect_true(is.na(result$attribute[4]))
expect_equal(result$attribute[5], "type")
expect_equal(result$attribute[6], "group")
expect_equal(result$attribute[7], "type")
expect_equal(result$attribute[8], "group")

# parse_ergm_model errors on non-ergm input
expect_error(parse_ergm_model(list()))
expect_error(parse_ergm_model(lm(1 ~ 1)))

# ---- Internal helpers --------------------------------------------------------

# .collect_rhs_terms handles nested + expressions
expr <- quote(a + b + c)
terms <- tabulergm:::.collect_rhs_terms(expr)
nms <- vapply(terms, as.character, character(1))
expect_equal(nms, c("a", "b", "c"))

# .parse_single_term extracts name from a symbol
result <- tabulergm:::.parse_single_term(quote(edges))
expect_equal(result$name, "edges")
expect_equal(length(result$attributes), 0L)

# .parse_single_term extracts name and attribute from a call
result <- tabulergm:::.parse_single_term(quote(nodematch("gender")))
expect_equal(result$name, "nodematch")
expect_equal(result$attributes, "gender")

# .parse_single_term extracts multiple attributes
result <- tabulergm:::.parse_single_term(quote(nodemix("race", "gender")))
expect_equal(result$name, "nodemix")
expect_equal(result$attributes, c("race", "gender"))

# .match_coef_to_term matches exact and prefix names
terms <- c("edges", "nodematch", "nodefactor")
expect_equal(tabulergm:::.match_coef_to_term("edges", terms), "edges")
expect_equal(tabulergm:::.match_coef_to_term("nodematch.gender", terms),
             "nodematch")
expect_equal(tabulergm:::.match_coef_to_term("nodefactor.race.Black", terms),
             "nodefactor")
expect_true(is.na(tabulergm:::.match_coef_to_term("unknownterm", terms)))

# .match_coef_to_term handles mixing term coefficient names
# "mix.race.A.B" should NOT match "nodemix" since "mix" != "nodemix"
terms <- c("edges", "nodemix")
expect_equal(tabulergm:::.match_coef_to_term("mix.race.A.B", terms),
             NA_character_)
expect_equal(tabulergm:::.match_coef_to_term("nodemix.race.A.B", terms),
             "nodemix")

# .match_coef_to_term handles non-dot prefix (e.g., b1star2 from b1star(2))
terms <- c("edges", "b1star")
expect_equal(tabulergm:::.match_coef_to_term("b1star2", terms), "b1star")

# ---- Model parsing (uses pre-fitted ergm objects) ----------------------------

if (requireNamespace("network", quietly = TRUE) &&
    requireNamespace("ergm", quietly = TRUE)) {

  library(network)
  library(ergm)

  # Load pre-fitted models (generated by data-raw/fit_ergm_models.R)
  fit      <- readRDS(system.file("fits", "fit_edges.rds", package = "tabulergm"))
  fit2     <- readRDS(system.file("fits", "fit_nodematch.rds", package = "tabulergm"))
  fit3     <- readRDS(system.file("fits", "fit_nodematch_diff.rds", package = "tabulergm"))
  fit4     <- readRDS(system.file("fits", "fit_nodemix.rds", package = "tabulergm"))
  fit_bip  <- readRDS(system.file("fits", "fit_bipartite.rds", package = "tabulergm"))
  fit_bip_dsp <- readRDS(system.file("fits", "fit_bipartite_dsp.rds", package = "tabulergm"))
  fit_bip_factor <- readRDS(system.file("fits", "fit_bipartite_factor.rds", package = "tabulergm"))
  fit_bip_match <- readRDS(system.file("fits", "fit_bipartite_match.rds", package = "tabulergm"))
  fit_nc_twice <- readRDS(system.file("fits", "fit_nodecov_twice.rds", package = "tabulergm"))

  # parse_ergm_model works with a simple edges-only model
  result <- parse_ergm_model(fit)
  expect_inherits(result, "data.frame")
  expect_true(all(c("term", "coef_name", "attribute", "estimate",
                     "se", "pvalue", "description", "math", "figure")
                   %in% names(result)))
  expect_equal(result$term, "edges")
  expect_equal(result$coef_name, "edges")
  expect_true(is.numeric(result$estimate))
  expect_true(is.numeric(result$se))
  expect_true(is.numeric(result$pvalue))
  # edges is a well-known term; expect a non-NA description
  expect_false(is.na(result$description[result$term == "edges"]))

  # parse_ergm_model maps expanded nodematch terms
  result2 <- parse_ergm_model(fit2)
  expect_true(nrow(result2) >= 2L)
  expect_true("edges" %in% result2$term)
  expect_true("nodematch" %in% result2$term)
  nm_row <- result2[result2$term == "nodematch", ]
  expect_equal(nm_row$attribute[1], "group")

  # parse_ergm_model works with nodematch diff=TRUE (multiple coefficients)
  result3 <- parse_ergm_model(fit3)
  # With diff=TRUE, nodematch expands to one coef per level
  nm_rows <- result3[result3$term == "nodematch", ]
  expect_true(nrow(nm_rows) >= 2L)
  expect_true(all(nm_rows$attribute == "color"))

  # parse_ergm_model handles mixing terms (nodemix)
  result4 <- parse_ergm_model(fit4)
  expect_true("nodemix" %in% result4$term)
  mix_rows <- result4[result4$term == "nodemix", ]
  # nodemix produces multiple coefficients for mixing categories
  expect_true(nrow(mix_rows) >= 1L)
  expect_true(all(mix_rows$attribute == "role"))

  # parse_ergm_model maps repeated terms to occurrence-specific attributes
  # (regression: attributes used to be joined by term name, so duplicate
  # terms all reported the first occurrence's attribute)
  result_nc <- parse_ergm_model(fit_nc_twice)
  expect_equal(result_nc$term, c("edges", "nodecov", "nodecov"))
  expect_equal(
    result_nc$coef_name,
    c("edges", "nodecov.wealth", "nodecov.priorates")
  )
  expect_equal(
    result_nc$attribute,
    c(NA_character_, "wealth", "priorates")
  )

  # parse_ergm_model handles bipartite network with b1star
  result_bip <- parse_ergm_model(fit_bip)
  expect_true("edges" %in% result_bip$term)
  expect_true("b1star" %in% result_bip$term)

  # parse_ergm_model handles bipartite gwb*dsp terms
  result_bip_dsp <- parse_ergm_model(fit_bip_dsp)
  expect_true("gwb1dsp" %in% result_bip_dsp$term)
  expect_true("gwb2dsp" %in% result_bip_dsp$term)
  expect_false(is.na(result_bip_dsp$math[result_bip_dsp$term == "gwb1dsp"][1]))
  expect_false(is.na(result_bip_dsp$math[result_bip_dsp$term == "gwb2dsp"][1]))

  # parse_ergm_model handles bipartite factor terms
  result_bip_factor <- parse_ergm_model(fit_bip_factor)
  expect_true("b1factor" %in% result_bip_factor$term)
  expect_true("b2factor" %in% result_bip_factor$term)
  expect_true(all(result_bip_factor$attribute[result_bip_factor$term == "b1factor"] == "a"))
  expect_true(all(result_bip_factor$attribute[result_bip_factor$term == "b2factor"] == "b"))
  expect_false(is.na(result_bip_factor$math[result_bip_factor$term == "b1factor"][1]))
  expect_false(is.na(result_bip_factor$math[result_bip_factor$term == "b2factor"][1]))

  # parse_ergm_model handles bipartite nodematch terms
  result_bip_match <- parse_ergm_model(fit_bip_match)
  expect_true("b1nodematch" %in% result_bip_match$term)
  expect_true("b2nodematch" %in% result_bip_match$term)
  expect_true(all(result_bip_match$attribute[result_bip_match$term == "b1nodematch"] == "a"))
  expect_true(all(result_bip_match$attribute[result_bip_match$term == "b2nodematch"] == "b"))
  expect_false(is.na(result_bip_match$math[result_bip_match$term == "b1nodematch"][1]))
  expect_false(is.na(result_bip_match$math[result_bip_match$term == "b2nodematch"][1]))
}
