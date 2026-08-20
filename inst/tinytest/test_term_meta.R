# Tests for optional YAML title/description/citation metadata, the
# override.* arguments, and citation markers/footnotes.

# ---- Citation normalization -------------------------------------------------

# A bare key becomes an entry with no identifier
cites <- tabulergm:::.normalize_citations("hunter2007")
expect_equal(length(cites), 1L)
expect_equal(cites[[1L]]$key, "hunter2007")
expect_equal(length(cites[[1L]]$ids), 0L)

# A prefixed identifier derives its key from the identifier
cites <- tabulergm:::.normalize_citations("doi:10.1234/abcd")
expect_equal(cites[[1L]]$key, "10.1234/abcd")
expect_equal(cites[[1L]]$ids[[1L]]$label, "doi:10.1234/abcd")
expect_equal(cites[[1L]]$ids[[1L]]$url, "https://doi.org/10.1234/abcd")

# A single entry list is accepted without wrapping
cites <- tabulergm:::.normalize_citations(list(key = "a2020", doi = "10.1/x"))
expect_equal(length(cites), 1L)
expect_equal(cites[[1L]]$key, "a2020")

# A list of entries stays a list of entries
cites <- tabulergm:::.normalize_citations(list(
  list(key = "a2020", doi = "10.1/x"),
  list(key = "b2021", arxiv = "1234.5678")
))
expect_equal(length(cites), 2L)
expect_equal(cites[[2L]]$ids[[1L]]$label, "arXiv:1234.5678")
expect_equal(cites[[2L]]$ids[[1L]]$url, "https://arxiv.org/abs/1234.5678")

# PubMed and plain URLs resolve too
cites <- tabulergm:::.normalize_citations(list(key = "c", pmid = "12345678"))
expect_equal(cites[[1L]]$ids[[1L]]$label, "PMID:12345678")
expect_equal(cites[[1L]]$ids[[1L]]$url,
  "https://pubmed.ncbi.nlm.nih.gov/12345678/")

cites <- tabulergm:::.normalize_citations("https://example.org/paper")
expect_equal(cites[[1L]]$ids[[1L]]$url, "https://example.org/paper")

# NULL and NA yield no citations
expect_equal(length(tabulergm:::.normalize_citations(NULL)), 0L)
expect_equal(length(tabulergm:::.normalize_citations(NA)), 0L)

# A non-list, non-character value is rejected
expect_error(tabulergm:::.normalize_citations(42))

# The bibliography deduplicates by key, keeping first-seen order
bib <- tabulergm:::.citation_bibliography(list(
  tabulergm:::.normalize_citations("b2021"),
  tabulergm:::.normalize_citations("a2020"),
  tabulergm:::.normalize_citations("b2021")
))
expect_equal(length(bib), 2L)
expect_equal(vapply(bib, `[[`, character(1), "key"), c("b2021", "a2020"))


# ---- Citation footnote rendering --------------------------------------------

entry <- tabulergm:::.normalize_citations(
  list(key = "hunter2007", doi = "10.1016/j.socnet.2006.08.005")
)

md <- tabulergm:::.render_citation_notes(entry, "markdown")
expect_equal(length(md), 1L)
# Brackets are escaped so Markdown does not read [key] as a reference link
expect_true(grepl("\\[hunter2007\\]", md, fixed = TRUE))
expect_true(grepl("(https://doi.org/10.1016/j.socnet.2006.08.005)", md,
  fixed = TRUE))

html <- tabulergm:::.render_citation_notes(entry, "html")
expect_true(grepl('<a href="https://doi.org/', html, fixed = TRUE))

tex <- tabulergm:::.render_citation_notes(entry, "latex")
expect_true(grepl("[hunter2007] doi:10.1016/j.socnet.2006.08.005", tex,
  fixed = TRUE))

# Free text is included ahead of the identifier
entry_text <- tabulergm:::.normalize_citations(
  list(key = "k", text = "Author, A. (2020). Title.", doi = "10.1/x")
)
tex <- tabulergm:::.render_citation_notes(entry_text, "latex")
expect_true(grepl("Author, A. (2020). Title.", tex, fixed = TRUE))

# A key with no identifier still renders
bare <- tabulergm:::.render_citation_notes(
  tabulergm:::.normalize_citations("solo"), "latex"
)
expect_equal(bare, "[solo]")

# An empty bibliography renders nothing
expect_equal(length(tabulergm:::.render_citation_notes(list(), "markdown")), 0L)
expect_equal(length(tabulergm:::.render_citation_notes(NULL, "markdown")), 0L)


# ---- YAML metadata reading --------------------------------------------------

# Shipped terms carry a curated title and description
data <- tabulergm:::.get_term_yml_data("edges", directed = FALSE)
expect_false(is.na(data$title))
expect_false(is.na(data$description))
expect_equal(length(data$citation), 0L)

# Terms with a known source carry a citation
data <- tabulergm:::.get_term_yml_data("gwesp", directed = FALSE)
expect_equal(length(data$citation), 1L)
expect_equal(data$citation[[1L]]$key, "hunter2007")

# Terms with two sources carry both, in file order
data <- tabulergm:::.get_term_yml_data("gwdegree", directed = FALSE)
expect_equal(vapply(data$citation, `[[`, character(1), "key"),
  c("snijders2006", "hunter2007"))

# Unknown terms yield empty metadata rather than an error
data <- tabulergm:::.get_term_yml_data("nonexistent_term_xyz", directed = FALSE)
expect_true(is.na(data$title))
expect_true(is.na(data$description))
expect_equal(length(data$citation), 0L)

# Every shipped term file defines a title and a description
for (f in list.files(system.file("terms", package = "tabulergm"),
                     full.names = TRUE)) {
  yml <- yaml::read_yaml(f, handlers = list(
    "bool#yes" = function(x) x, "bool#no" = function(x) x
  ))
  expect_true(is.character(yml$title) && nzchar(yml$title),
    info = sprintf("title present in %s", basename(f)))
  expect_true(is.character(yml$description) && nzchar(yml$description),
    info = sprintf("description present in %s", basename(f)))
}


# ---- YAML wins over the ergm term database ----------------------------------

res <- parse_ergm_formula(~ edges, directed = FALSE)
yml <- tabulergm:::.get_term_yml_data("edges", directed = FALSE)
expect_equal(res$title[1L], yml$title)
expect_equal(res$description[1L], yml$description)

# Terms without a YAML file fall back to the ergm database, which supplies
# a title and a (longer) description
res <- parse_ergm_formula(~ kstar(2), directed = FALSE)
expect_false(is.na(res$title[1L]))
expect_false(is.na(res$description[1L]))
expect_true(is.na(res$citation[1L]))


# ---- Override normalization -------------------------------------------------

ov <- tabulergm:::.normalize_overrides(
  override.title = c(edges = "Density"),
  override.desc  = c(edges = "Baseline.")
)
expect_equal(ov$edges$title, "Density")
expect_equal(ov$edges$description, "Baseline.")

# The bulk list accepts `desc` as an alias for `description`
ov <- tabulergm:::.normalize_overrides(
  override = list(edges = list(desc = "Baseline."))
)
expect_equal(ov$edges$description, "Baseline.")

# Per-field arguments win over the bulk list
ov <- tabulergm:::.normalize_overrides(
  override = list(edges = list(title = "Bulk")),
  override.title = c(edges = "Field")
)
expect_equal(ov$edges$title, "Field")

# ... and merge with, rather than replace, the other bulk fields
ov <- tabulergm:::.normalize_overrides(
  override = list(edges = list(title = "Bulk", math = "x")),
  override.title = c(edges = "Field")
)
expect_equal(ov$edges$title, "Field")
expect_equal(ov$edges$math, "x")

# Unnamed vectors, unknown fields, and bad shapes are rejected
expect_error(tabulergm:::.normalize_overrides(override.title = "unnamed"))
expect_error(tabulergm:::.normalize_overrides(
  override = list(edges = list(bogus = "x"))))
expect_error(tabulergm:::.normalize_overrides(override = list("unnamed")))
expect_error(tabulergm:::.normalize_overrides(override = list(edges = "flat")))
expect_error(tabulergm:::.normalize_overrides(
  override = list(edges = list(title = c("two", "values")))))
expect_error(tabulergm:::.normalize_overrides(
  override = list(edges = list(title = NA_character_))))


# ---- Overrides applied through the parsers ----------------------------------

res <- parse_ergm_formula(
  ~ edges + triangle, directed = FALSE,
  override.title = c(edges = "Density"),
  override.desc  = c(edges = "Baseline propensity."),
  override.math  = c(edges = "n_e")
)
expect_equal(res$title[res$term == "edges"], "Density")
expect_equal(res$description[res$term == "edges"], "Baseline propensity.")
expect_equal(res$math[res$term == "edges"], "n_e")
# Other terms keep their dictionary values
expect_equal(res$title[res$term == "triangle"], "Triangles")

# Citation overrides replace the dictionary citation
res <- parse_ergm_formula(
  ~ triangle, directed = FALSE,
  override.citation = list(triangle = list(key = "mine2026", doi = "10.1/z"))
)
expect_equal(res$citation[1L], "mine2026")
expect_equal(attr(res, "tabulergm_citations")[[1L]]$key, "mine2026")

# A citation can be added to a term that has none
res <- parse_ergm_formula(
  ~ edges, directed = FALSE,
  override.citation = list(edges = "doi:10.1/y")
)
expect_equal(res$citation[1L], "10.1/y")

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
  expect_true(grepl("(mcpherson2001)", tbl$term[tbl$term != "edges"][1L],
    fixed = TRUE))

  # The bibliography rides along on data.frame output for tabulergm_save()
  expect_equal(length(attr(tbl, "tabulergm_citations")), 1L)

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
  tabulergm_table(~ edges, directed = FALSE, format = "markdown")
)
expect_false(any(grepl("frank1986", md, fixed = TRUE)))

# Terms with no citation get no marker
tbl <- tabulergm_table(~ edges, directed = FALSE)
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
