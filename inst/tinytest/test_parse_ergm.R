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

# parse_ergm_formula errors on non-formula input
expect_error(parse_ergm_formula("not a formula"))
expect_error(parse_ergm_formula(42))

# parse_ergm_formula handles bipartite terms
f <- y ~ edges + b1star(2) + b2factor("type")
result <- parse_ergm_formula(f)
expect_equal(result$term, c("edges", "b1star", "b2factor"))
expect_true(is.na(result$attribute[1]))
expect_true(is.na(result$attribute[2]))
expect_equal(result$attribute[3], "type")

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
terms <- c("edges", "nodemix")
expect_equal(tabulergm:::.match_coef_to_term("mix.race.A.B", terms),
             NA_character_)
expect_equal(tabulergm:::.match_coef_to_term("nodemix.race.A.B", terms),
             "nodemix")

# ---- Model parsing (requires ergm and network) -------------------------------

if (requireNamespace("network", quietly = TRUE) &&
    requireNamespace("ergm", quietly = TRUE)) {

  library(network)
  library(ergm)

  # parse_ergm_model works with a simple edges-only model
  nw <- network(10, directed = FALSE, density = 0.3)
  suppressWarnings(fit <- ergm(nw ~ edges))
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
  nw2 <- network(10, directed = FALSE, density = 0.3)
  set.vertex.attribute(nw2, "group", rep(c("A", "B"), 5))
  suppressWarnings(fit2 <- ergm(nw2 ~ edges + nodematch("group")))
  result2 <- parse_ergm_model(fit2)
  expect_true(nrow(result2) >= 2L)
  expect_true("edges" %in% result2$term)
  expect_true("nodematch" %in% result2$term)
  nm_row <- result2[result2$term == "nodematch", ]
  expect_equal(nm_row$attribute[1], "group")

  # parse_ergm_model works with nodematch diff=TRUE (multiple coefficients)
  nw3 <- network(10, directed = FALSE, density = 0.3)
  set.vertex.attribute(nw3, "color", rep(c("red", "blue"), 5))
  suppressWarnings(
    fit3 <- ergm(nw3 ~ edges + nodematch("color", diff = TRUE))
  )
  result3 <- parse_ergm_model(fit3)
  # With diff=TRUE, nodematch expands to one coef per level
  nm_rows <- result3[result3$term == "nodematch", ]
  expect_true(nrow(nm_rows) >= 2L)
  expect_true(all(nm_rows$attribute == "color"))

  # parse_ergm_model handles mixing terms (nodemix)
  nw4 <- network(20, directed = TRUE, density = 0.2)
  set.vertex.attribute(nw4, "role", rep(c("A", "B"), 10))
  suppressWarnings(
    fit4 <- ergm(nw4 ~ edges + nodemix("role"))
  )
  result4 <- parse_ergm_model(fit4)
  expect_true("nodemix" %in% result4$term)
  mix_rows <- result4[result4$term == "nodemix", ]
  # nodemix produces multiple coefficients for mixing categories
  expect_true(nrow(mix_rows) >= 1L)
  expect_true(all(mix_rows$attribute == "role"))

  # parse_ergm_model handles bipartite network with b1star
  bip <- network(10, directed = FALSE, bipartite = 4, density = 0.3)
  suppressWarnings(
    fit_bip <- ergm(bip ~ edges + b1star(2))
  )
  result_bip <- parse_ergm_model(fit_bip)
  expect_true("edges" %in% result_bip$term)
  expect_true("b1star" %in% result_bip$term)
}
