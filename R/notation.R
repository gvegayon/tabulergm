# ---- Term Definition Standards ----
#
# Documentation-only file describing the notation and drawing standards
# used in the YAML term database (inst/terms/). See ?"tabulergm-notation".


#' Notation and Drawing Standards for Term Definitions
#'
#' @description
#' Reference for the conventions used in the YAML term database under
#' `inst/terms/`. Follow these standards when adding or editing term
#' definitions so that the math and figures stay consistent across the
#' term dictionary.
#'
#' @section Files:
#' Each term lives in `inst/terms/<term>.<directed|undirected>.yml`, where
#' `<term>` is the canonical `ergm` term name as written in a model formula
#' (e.g., `gwesp`, `b1nodematch`). Terms available for both directed and
#' undirected networks get one file per variant; bipartite terms use
#' `undirected`. A file has up to five top-level entries: `title` and
#' `description` (text, both optional), `citation` (optional, see below),
#' `plot` (the network drawing specification), and `math` (a LaTeX
#' expression). No parser changes are needed for new terms: files are
#' looked up by term name.
#'
#' @section Titles, descriptions, and citations:
#' `title` is a short label, capitalized like a heading and without a
#' trailing period (e.g. `Uniform homophily`). `description` is one to
#' three sentences of prose saying what the statistic counts and why a
#' modeler would include it; write it as a folded block scalar (`>-`) so
#' the source stays readable. Both fields are optional: when absent,
#' `tabulergm` falls back to the `title` and `description` recorded in the
#' `ergm` term database, which are accurate but often too long, too
#' technical, or full of raw LaTeX for a table cell. Prefer writing them
#' here.
#'
#' `citation` records the source(s) that introduced the term. Each entry
#' carries a `key` (the marker shown in the table, conventionally
#' `lastnameYEAR`) and, where one exists, a machine-readable identifier so
#' readers can pull the full reference into their own bibliography:
#'
#' \preformatted{citation:
#'   - key: hunter2007
#'     doi: 10.1016/j.socnet.2006.08.005
#' }
#'
#' Accepted identifier fields are `doi`, `arxiv`, `pmid`, and `url`; an
#' entry may also carry free-text `text`, which is the right choice when
#' no stable identifier can be verified. \strong{Always resolve an
#' identifier before committing it} (for a DOI, `https://doi.org/<id>`);
#' an identifier that points at the wrong paper is worse than none.
#'
#' `tabulergm_table()` renders a `(key)` marker next to the term's
#' description and a matching `[key] identifier` line below the table. Users
#' can replace any of these fields per table through the `override`
#' arguments of [tabulergm_table()] without editing the YAML.
#'
#' @section Math notation:
#' \itemize{
#'   \item \eqn{y_{ij}}: tie indicator. Undirected statistics sum over
#'     \eqn{i<j}; directed statistics sum over \eqn{i \neq j}.
#'   \item \eqn{x_i}: vertex attribute value of node \eqn{i};
#'     \eqn{x_{ij}}: dyadic covariate value (e.g., `edgecov`).
#'   \item \eqn{\mathbf{1}(\cdot)}: indicator function (`\mathbf{1}` in
#'     LaTeX).
#'   \item Attribute levels: \eqn{k} for a single level (factor terms),
#'     \eqn{(k, l)} for mixing pairs, and \eqn{(p, q)} for center/leaf
#'     values in star-mixing terms.
#'   \item Bipartite modes: \eqn{B_1} (first mode) and \eqn{B_2} (second
#'     mode); \eqn{n_{B_1}} and \eqn{n_{B_2}} for the mode sizes.
#'   \item Geometrically weighted terms follow the Hunter (2007)
#'     parameterization, e.g.
#'     \eqn{\exp(\tau) \sum_i [1 - (1 - e^{-\tau})^i] EP_i(y)}, where the
#'     exponent is the summation index and \eqn{EP_i}, \eqn{DP_i}, and
#'     \eqn{D_i} are the edgewise shared partner, dyadwise shared partner,
#'     and degree counts.
#'   \item Directed shared-partner terms carry the two-path type as a
#'     superscript, e.g. \eqn{EP^{\mathrm{OTP}}_i}, because `ergm` counts
#'     outgoing two-paths (\code{OTP}) by default and `gwesp`/`gwdsp` take a
#'     \code{type} argument that changes which two-paths are counted.
#' }
#' Always verify a new formula against the `ergm` term manual (see
#' [ergm::ergmTerm] and [ergm::search.ergmTerms()]) and the source
#' literature. When the manual is ambiguous, compare the formula
#' numerically against `summary(nw ~ term)` on a small test network.
#'
#' @section Drawing conventions:
#' The `plot` entry supports `edgelist`, `vcolor`, `vshape`, `vsize`,
#' `ecolor`, `elinetype`, and `layout` (with `x` and `y` coordinates).
#' Edgelists are chains like `"0->1->2, 0->3"`: each consecutive pair is
#' one edge. Per-vertex vectors follow the node order obtained from the
#' parsed edgelist (unique node ids, all tail nodes first, then head
#' nodes); render the figure to double-check the mapping.
#' \itemize{
#'   \item \strong{Vertex color}: `black` marks the focal structure of a
#'     term; `gray` marks non-focal context, both attribute-irrelevant
#'     nodes and structurally non-focal nodes (e.g., the shared partners
#'     in `gwesp`/`gwdsp`); `orange` marks nodes whose attribute enters
#'     the statistic (matched pairs share orange); mixing terms use
#'     `orange` vs. teal (`"#008080"`, quoted in the YAML) for the two
#'     attribute categories, a colorblind-friendly pairing. These colors
#'     drive the explanatory notes appended below rendered tables, so
#'     use them consistently.
#'   \item \strong{Vertex shape}: `square` marks first-mode (\eqn{B_1})
#'     nodes and `circle` marks second-mode (\eqn{B_2}) nodes in bipartite
#'     drawings. One-mode drawings use circles only.
#'   \item \strong{Layout}: bipartite drawings place the first mode on the
#'     left and the second mode on the right.
#'   \item \strong{Vertex size}: `1.0` for focal or attribute-relevant
#'     nodes, `.5` for context nodes.
#'   \item \strong{Edges}: solid black for focal ties; `gray` for
#'     context ties (e.g., the two-paths in shared-partner terms);
#'     dashed (`elinetype: 2`) for match/covariate annotations;
#'     `orange` when the edge itself carries the covariate
#'     (e.g., `edgecov`).
#'   \item Directedness comes from the file name; arrows are drawn
#'     automatically for `.directed.yml` terms.
#' }
#'
#' @section Checklist for a new term:
#' \enumerate{
#'   \item Add the YAML file(s) following the standards above, including a
#'     `title` and `description`, and a `citation` when the term has an
#'     identifiable source.
#'   \item Add tinytest coverage in `inst/tinytest/test_term_db.R`
#'     (file lookup, math/figure reading, and formula integration).
#'   \item Add the term to the dictionary tables in `README.qmd` and
#'     `vignettes/ergm-with-tabulergm.Rmd`; both contain a hidden
#'     coverage-check chunk that fails the render if a term is missing.
#'   \item Re-render the README, and run the tests and `devtools::check()`.
#' }
#'
#' @references
#' Hunter, D. R. (2007). Curved exponential family models for social
#' networks. \emph{Social Networks}, 29(2), 216--230.
#' \doi{10.1016/j.socnet.2006.08.005}
#'
#' Bomiriya, R. P., Bansal, S., & Hunter, D. R. (2014). Modeling homophily
#' in ERGMs for bipartite networks. (No stable identifier verified; the
#' arXiv id previously given here, 1412.1151, belongs to an unrelated
#' paper.)
#'
#' @name tabulergm-notation
#' @keywords internal
NULL
