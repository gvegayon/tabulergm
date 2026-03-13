# -- Formula parsing (no ergm needed) ------------------------------------------

test_that("parse_ergm_formula works with a simple formula", {
  f <- y ~ edges + triangle
  result <- parse_ergm_formula(f)

  expect_s3_class(result, "data.frame")
  expect_equal(result$term, c("edges", "triangle"))
  expect_true(all(is.na(result$attribute)))
  expect_true(all(is.na(result$estimate)))
  expect_true(all(is.na(result$se)))
  expect_true(all(is.na(result$pvalue)))
  expect_true(all(c("description", "math", "figure") %in% names(result)))
})

test_that("parse_ergm_formula extracts attributes from terms", {
  f <- y ~ edges + nodematch("gender") + nodefactor("race")
  result <- parse_ergm_formula(f)

  expect_equal(result$term, c("edges", "nodematch", "nodefactor"))
  expect_equal(result$attribute, c(NA_character_, "gender", "race"))
})

test_that("parse_ergm_formula works with a single term", {
  f <- y ~ edges
  result <- parse_ergm_formula(f)

  expect_equal(nrow(result), 1L)
  expect_equal(result$term, "edges")
  expect_true(is.na(result$attribute))
})

test_that("parse_ergm_formula handles one-sided formulas", {
  f <- ~ edges + triangle
  result <- parse_ergm_formula(f)

  expect_equal(result$term, c("edges", "triangle"))
})

test_that("parse_ergm_formula handles offset() terms", {
  f <- y ~ edges + offset(nodematch("gender"))
  result <- parse_ergm_formula(f)

  expect_equal(result$term, c("edges", "offset(nodematch)"))
  expect_equal(result$attribute, c(NA_character_, "gender"))
})

test_that("parse_ergm_formula handles curved terms", {
  f <- y ~ edges + gwesp(0.5, fixed = TRUE)
  result <- parse_ergm_formula(f)

  expect_equal(result$term, c("edges", "gwesp"))
  expect_true(is.na(result$attribute[2]))
})

test_that("parse_ergm_formula errors on non-formula input", {
  expect_error(parse_ergm_formula("not a formula"), "formula")
  expect_error(parse_ergm_formula(42), "formula")
})

# -- Model parsing (requires ergm) --------------------------------------------

test_that("parse_ergm_model errors on non-ergm input", {
  expect_error(parse_ergm_model(list()), "ergm")
  expect_error(parse_ergm_model(lm(1 ~ 1)), "ergm")
})

test_that("parse_ergm_model works with a simple fitted model", {
  skip_if_not_installed("network")
  skip_if_not_installed("ergm")

  nw <- network::network(10, directed = FALSE, density = 0.3)

  suppressWarnings(
    fit <- ergm::ergm(nw ~ edges)
  )

  result <- parse_ergm_model(fit)

  expect_s3_class(result, "data.frame")
  expect_true(all(c("term", "coef_name", "attribute", "estimate",
                     "se", "pvalue", "description", "math", "figure")
                   %in% names(result)))
  expect_equal(result$term, "edges")
  expect_equal(result$coef_name, "edges")
  expect_true(is.numeric(result$estimate))
  expect_true(is.numeric(result$se))
  expect_true(is.numeric(result$pvalue))
})

test_that("parse_ergm_model maps expanded terms back to formula terms", {
  skip_if_not_installed("network")
  skip_if_not_installed("ergm")

  nw <- network::network(10, directed = FALSE, density = 0.3)
  network::set.vertex.attribute(nw, "group", rep(c("A", "B"), 5))

  suppressWarnings(
    fit <- ergm::ergm(nw ~ edges + nodematch("group"))
  )

  result <- parse_ergm_model(fit)

  expect_true(nrow(result) >= 2L)
  expect_true("edges" %in% result$term)
  expect_true("nodematch" %in% result$term)

  nm_row <- result[result$term == "nodematch", ]
  expect_equal(nm_row$attribute[1], "group")
})

test_that("parse_ergm_model returns metadata columns", {
  skip_if_not_installed("network")
  skip_if_not_installed("ergm")

  nw <- network::network(10, directed = FALSE, density = 0.3)

  suppressWarnings(
    fit <- ergm::ergm(nw ~ edges)
  )

  result <- parse_ergm_model(fit)

  # description should come from the ergm term database
  expect_true("description" %in% names(result))
  # edges is a well-known term; expect a non-NA description
  expect_false(is.na(result$description[result$term == "edges"]))
})

# -- Internal helpers ----------------------------------------------------------

test_that(".collect_rhs_terms handles nested + expressions", {
  expr <- quote(a + b + c)
  terms <- tabulergm:::.collect_rhs_terms(expr)
  names <- vapply(terms, as.character, character(1))
  expect_equal(names, c("a", "b", "c"))
})

test_that(".parse_single_term extracts name from a symbol", {
  result <- tabulergm:::.parse_single_term(quote(edges))
  expect_equal(result$name, "edges")
  expect_true(is.na(result$attribute))
})

test_that(".parse_single_term extracts name and attribute from a call", {
  result <- tabulergm:::.parse_single_term(quote(nodematch("gender")))
  expect_equal(result$name, "nodematch")
  expect_equal(result$attribute, "gender")
})

test_that(".match_coef_to_term matches exact and prefix names", {
  terms <- c("edges", "nodematch", "nodefactor")
  expect_equal(tabulergm:::.match_coef_to_term("edges", terms), "edges")
  expect_equal(tabulergm:::.match_coef_to_term("nodematch.gender", terms),
               "nodematch")
  expect_equal(tabulergm:::.match_coef_to_term("nodefactor.race.Black", terms),
               "nodefactor")
  expect_true(is.na(tabulergm:::.match_coef_to_term("unknownterm", terms)))
})
