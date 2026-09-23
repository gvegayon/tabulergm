# tabulergm 0.2.0

## User-facing changes

* Fixed a crash rendering a styled Markdown table (`with_style_name_over_formula()`)
  with a Windows-style `figures_dir`; `figures_dir` is now also validated when
  the table is built rather than when it is first rendered (#35).

* Fixed spurious `ergm` version-compatibility warnings when tabulating a
  fitted model (#35).

* Fitted-model tables round all numeric columns (estimates, standard errors,
  and p-values) to 2 decimal places by default; control this with `digits`
  (#34).

* `with_style_name_over_formula()` gained persistent `column_widths` and
  `figure_height` settings, honored across HTML, Markdown, previews, and
  saved LaTeX (#34).

* Fixed equations in compact Markdown tables so GitHub and other renderers
  do not receive double-escaped inequality operators inside TeX math (#30).

* Added composable table styles: `with_style_name_over_formula()` stacks
  curated titles over formulae with figures in a compact column;
  `with_style_plain()` restores the default layout (#30).

* Formula tables now honor `include_description = FALSE` (#30).

* Term YAML files gained optional `title`, `description`, and `citation`
  fields; all 35 shipped terms now carry curated text instead of falling
  back to the `ergm` database's (#24).

* Tables gained a `title` column (`include_title = TRUE`); `description`
  now holds prose instead of the short `ergm` title (#24).

* Added `override`/`override.*` arguments to `tabulergm_table()`,
  `parse_ergm_model()`, and `parse_ergm_formula()` for replacing any
  metadata field per term or per coefficient (#24).

* Cited terms now show a `(key)` marker with the matching reference listed
  below the table; nine shipped terms carry a citation (#24).

* Fixed an incorrect arXiv identifier in `?"tabulergm-notation"`: 1412.1151
  was cited for Bomiriya et al. (2014) but belongs to an unrelated paper
  (#24).

## Internal changes

* Added CRAN, download, license, and dependency badges to the README.

* The please-bump check now also verifies that `NEWS.md` matches
  `DESCRIPTION`.

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
