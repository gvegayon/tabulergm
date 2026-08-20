# tabulergm (development version)

* Term YAML files under `inst/terms/` now accept optional `title`,
  `description`, and `citation` entries. All 35 shipped terms carry a
  curated title and description; previously the table showed the `ergm`
  database's `title` in the `description` column.

* Tables gained a `title` column, shown with
  `tabulergm_table(include_title = TRUE)`. The `description` column now
  holds prose describing the term rather than the short `ergm` title.

* Any metadata field can be replaced per table with the new `override`
  arguments of `tabulergm_table()`, `parse_ergm_model()`, and
  `parse_ergm_formula()`: `override.title`, `override.desc`,
  `override.math`, `override.figure`, and `override.citation` take named
  vectors keyed by term, and `override` takes a list for editing several
  fields at once. Override names also match coefficient names, so a single
  expanded coefficient can be targeted.

* Terms with a `citation` now show a `(key)` marker next to their
  description, with the matching `[key] identifier` line appended below
  HTML, Markdown, and LaTeX tables. Citations record a DOI, arXiv id,
  PubMed id, or URL so readers can import the reference into their own
  bibliography. `gwesp`, `gwdsp`, `gwdegree`, `altkstar`, `triangle`,
  `mutual`, `nodematch`, `b1nodematch`, and `b2nodematch` ship with
  citations.

* Fixed an incorrect arXiv identifier in `?"tabulergm-notation"`: 1412.1151
  was cited for Bomiriya et al. (2014) but belongs to an unrelated paper.

# tabulergm 0.1.0

* First release.

* `tabulergm_table()` builds publication-ready tables from a fitted
  `ergm` object or from a bare ERGM formula, in `data.frame`,
  HTML, or Markdown form.

* `tabulergm_view()` opens a self-contained HTML preview of a table in the
  RStudio viewer or the system browser.

* `tabulergm_save()` writes Markdown and LaTeX table code together with the
  term figures, rewriting the figure paths so the exported table is
  self-contained.

* `parse_ergm_model()` and `parse_ergm_formula()` expose the underlying
  term metadata as a data frame.

* Ships a term dictionary covering 35 term/directedness combinations, each
  with a LaTeX definition and a network drawing. Figures are drawn with
  netplot and can be replaced through `tabulergm_set_plotfun()`.
