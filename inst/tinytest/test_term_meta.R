# Tests for optional YAML title/description/citation metadata, the
# override.* arguments, and citation markers/footnotes.

# ---- Citation specifications, as a user supplies them ----------------------

# Every accepted citation shape gets a marker next to its description and a
# footnote line below the table: key + DOI, arXiv, a bare prefixed DOI, PubMed
# with free text (which comes first), a bare key, and a plain URL. A key cited
# by two terms is listed once, in first-seen order.
cites <- list(
  edges      = list(list(key = "a2020", doi = "10.1/x"),
                    list(key = "b2021", arxiv = "1234.5678")),
  triangle   = "doi:10.1234/abcd",
  isolates   = list(key = "c", pmid = "12345678",
                    text = "Author, A. (2020). Title."),
  concurrent = "solo",
  kstar      = "https://example.org/paper",
  degree     = list(key = "a2020", doi = "10.1/x")
)
f <- ~ edges + triangle + isolates + concurrent + kstar(2) + degree(1)
tbl <- tabulergm_table(f, directed = FALSE, override.citation = cites)
expect_equal(sub(".* \\(", "(", tbl$description), c(
  "(a2020; b2021)", "(10.1234/abcd)", "(c)", "(solo)",
  "(https://example.org/paper)", "(a2020)"
))

md <- unlist(strsplit(as.character(tabulergm_table(
  f, directed = FALSE, override.citation = cites, format = "markdown"
)), "\n"))
# Brackets are escaped so Markdown does not read [key] as a reference link
expect_equal(trimws(grep("^\\*\\\\\\[", md, value = TRUE)), c(
  "*\\[a2020\\] [doi:10.1/x](https://doi.org/10.1/x)*",
  "*\\[b2021\\] [arXiv:1234.5678](https://arxiv.org/abs/1234.5678)*",
  "*\\[10.1234/abcd\\] [doi:10.1234/abcd](https://doi.org/10.1234/abcd)*",
  paste0("*\\[c\\] Author, A. (2020). Title. ",
         "[PMID:12345678](https://pubmed.ncbi.nlm.nih.gov/12345678/)*"),
  "*\\[solo\\]*",
  "*\\[https://example.org/paper\\] [https://example.org/paper](https://example.org/paper)*"
))

# NA removes a shipped citation; a value of the wrong type is rejected
res <- parse_ergm_formula(~ triangle, directed = FALSE,
  override.citation = list(triangle = NA))
expect_true(is.na(res$citation))
expect_error(parse_ergm_formula(~ edges, directed = FALSE,
  override.citation = list(edges = 42)), "citation")


# ---- Shipped term dictionary ------------------------------------------------

# Every shipped term file (aliases included) resolves by name and
# directedness, and yields a curated title, description, math, and a
# rendered drawing
for (f in list.files(system.file("terms", package = "tabulergm"))) {
  term <- sub("\\.(un)?directed\\.yml$", "", f)
  res <- parse_ergm_formula(stats::as.formula(paste("y ~", term)),
    directed = grepl("\\.directed\\.yml$", f))
  expect_true(!is.na(res$title) && nzchar(res$title),
    info = sprintf("title present for %s", f))
  expect_true(!is.na(res$description) && nzchar(res$description),
    info = sprintf("description present for %s", f))
  expect_false(is.na(res$math), info = sprintf("math present for %s", f))
  expect_true(file.exists(res$figure),
    info = sprintf("figure drawn for %s", f))
}

# YAML wins over the ergm term database, offset() resolves to the wrapped
# term, and a directed-only term is found without a directedness hint
res <- parse_ergm_formula(y ~ edges + offset(edges) + mutual)
expect_equal(res$title, c("Number of edges", "Number of edges",
                          "Reciprocated ties"))
expect_equal(res$math[2], res$math[1])
expect_true(grepl("y_{ji}", res$math[3], fixed = TRUE))

# Terms without a YAML file fall back to the ergm database, which supplies
# a title and a (longer) description, but no math or citation
res <- parse_ergm_formula(~ twopath, directed = FALSE)
expect_false(is.na(res$title[1L]))
expect_false(is.na(res$description[1L]))
expect_true(is.na(res$math[1L]))
expect_true(is.na(res$citation[1L]))


# ---- Overrides applied through the parsers ----------------------------------

# Per-field arguments and the bulk `override` list combine: the bulk list
# accepts `desc` for `description`, and a per-field argument wins over the
# bulk value without discarding the bulk list's other fields
res <- parse_ergm_formula(
  ~ edges + triangle + kstar(2), directed = FALSE,
  override = list(edges = list(title = "Bulk", desc = "Baseline.",
                               math = "n_e")),
  override.title = c(edges = "Density"),
  override.desc  = c(triangle = "Closure.")
)
expect_equal(res$title[1], "Density")
expect_equal(res$description[1:2], c("Baseline.", "Closure."))
expect_equal(res$math[1], "n_e")
# Other terms keep their dictionary values
expect_equal(res$title[3], "k-stars")

# Malformed overrides are rejected
bad <- list(
  list(override.title = "unnamed"),
  list(override = list(edges = list(bogus = "x"))),
  list(override = list("unnamed")),
  list(override = list(edges = "flat")),
  list(override = list(edges = list(title = c("two", "values")))),
  list(override = list(edges = list(title = NA_character_)))
)
for (args in bad) {
  expect_error(do.call(parse_ergm_formula,
    c(list(~ edges, directed = FALSE), args)),
    info = deparse(args))
}

# Citation overrides replace the dictionary citation
res <- parse_ergm_formula(
  ~ triangle, directed = FALSE,
  override.citation = list(triangle = list(key = "mine2026", doi = "10.1/z"))
)
expect_equal(res$citation[1L], "mine2026")
expect_equal(attr(res, "tabulergm_citations")[[1L]]$key, "mine2026")

# Overrides for absent terms warn rather than fail silently
expect_warning(
  parse_ergm_formula(~ edges, directed = FALSE,
    override.title = c(no_such_term = "x"))
)


# ---- Overrides target coefficients as well as terms -------------------------

if (requireNamespace("ergm", quietly = TRUE)) {
  fit <- readRDS(
    system.file("fits", "fit_nodematch_diff.rds", package = "tabulergm")
  )
  parsed <- parse_ergm_model(fit)
  expanded <- parsed$coef_name[parsed$term == "nodematch"]

  if (length(expanded) > 1L) {
    spec <- list("x")
    names(spec) <- expanded[[1L]]
    res <- parse_ergm_model(fit, override.title = unlist(spec))
    # Only the targeted coefficient row changed
    expect_equal(res$title[res$coef_name == expanded[[1L]]], "x")
    expect_false(identical(res$title[res$coef_name == expanded[[2L]]], "x"))
  }
}


# ---- Citation markers and footnotes in tables -------------------------------

# The marker lands on the description when that column is shown
tbl <- tabulergm_table(~ triangle, directed = FALSE)
expect_true(grepl("(frank1986)", tbl$description[1L], fixed = TRUE))

md <- as.character(
  tabulergm_table(~ triangle, directed = FALSE, format = "markdown")
)
expect_true(any(grepl("frank1986", md, fixed = TRUE)))
expect_true(any(grepl("https://doi.org/10.1080/01621459.1986.10478342", md,
  fixed = TRUE)))

# ... and on the term when the table has no description column
if (requireNamespace("ergm", quietly = TRUE)) {
  fit <- readRDS(
    system.file("fits", "fit_nodematch.rds", package = "tabulergm")
  )
  tbl <- tabulergm_table(fit)
  expect_false("description" %in% names(tbl))
  expect_true(grepl("(wasserman1996; mcpherson2001)",
    tbl$term[startsWith(tbl$term, "nodematch")][1L], fixed = TRUE))

  # The bibliography rides along on data.frame output for tabulergm_save()
  expect_equal(
    vapply(attr(tbl, "tabulergm_citations"), `[[`, "", "key"),
    c("holland1981", "wasserman1996", "mcpherson2001")
  )

  # include_title places the title immediately after term
  tbl <- tabulergm_table(fit, include_title = TRUE)
  expect_equal(names(tbl)[1:2], c("term", "title"))
}

# Several citations on one term share a single marker
tbl <- tabulergm_table(~ gwdegree(0.5, fixed = TRUE), directed = FALSE)
expect_true(grepl("(snijders2006; hunter2007)", tbl$description[1L],
  fixed = TRUE))

# Only the citations actually used are listed
md <- as.character(
  tabulergm_table(~ isolates, directed = FALSE, format = "markdown")
)
expect_false(any(grepl("frank1986", md, fixed = TRUE)))

# Terms with no citation get no marker
tbl <- tabulergm_table(~ concurrent, directed = FALSE)
expect_false(grepl("(", tbl$description[1L], fixed = TRUE))

# HTML output carries linked identifiers below the table
html <- as.character(
  tabulergm_table(~ triangle, directed = FALSE, format = "html")
)
expect_true(any(grepl('class="tabulergm-citations"', html, fixed = TRUE)))
expect_true(any(grepl('<a href="https://doi.org/', html, fixed = TRUE)))


# ---- Citation footnotes in exported files ------------------------------------

if (requireNamespace("knitr", quietly = TRUE)) {
  out_dir <- tempfile("tabulergm-cite-")
  tabulergm_save(~ triangle, out_dir, include_math = TRUE)

  md <- readLines(file.path(out_dir, "tabulergm-table.md"))
  expect_true(any(grepl("frank1986", md, fixed = TRUE)))

  tex <- readLines(file.path(out_dir, "tabulergm-table.tex"))
  expect_true(any(grepl("\\emph{[frank1986]", tex, fixed = TRUE)))

  # A hand-built data frame has no bibliography and exports cleanly
  plain <- data.frame(term = "edges", estimate = 1, stringsAsFactors = FALSE)
  out_dir2 <- tempfile("tabulergm-plain-")
  expect_silent(tabulergm_save(plain, out_dir2))
}
