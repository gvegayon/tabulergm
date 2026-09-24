# tabulergm 0.2.0

## User-facing changes

* Audited term citations: each term now lists the paper that introduced it
  as an ERGM/p* statistic first (e.g. Wasserman and Pattison 1996 for
  `nodematch`), followed by at most one theory reference (#38).

* Fixed incorrect citation identifiers for `b1nodematch`/`b2nodematch` and
  in the `parse_ergm_model()`/`parse_ergm_formula()` documentation (#38).

* `edges`, `nodemix`, `edgecov`, `istar`, `ostar`, `gwb1dsp`, and `gwb2dsp`
  now carry citations; directed `triangle` and the bipartite and directed
  geometrically weighted terms cite their directed or bipartite sources (#38).

* Markdown figure files are named after the bare term, no longer picking up
  citation markers such as `triangle-frank1986.png` (#38).

* Added 14 terms to the term dictionary: `gwidegree`, `gwodegree`,
  `gwb1degree`, `gwb2degree`, `nodeifactor`, `nodeofactor`, `kstar`,
  `istar`, `ostar`, `isolates`, `degree`, `concurrent`, `dgwesp`, and
  `dgwdsp` (#37).

* The `gwdegree` and `altkstar` drawings now follow the color and size
  legend (#37).

* Term YAML files can reuse another term's entry with `alias: <term>`,
  overriding individual fields as needed; `dgwesp` and `dgwdsp` are now
  aliases of `gwesp` and `gwdsp` (#37).

* Term drawings can include isolated nodes: a lone node id in the YAML
  `edgelist` (e.g. `"1->2, 0"`) adds a node with no ties (#37).

* Fixed a crash rendering a styled Markdown table (`with_style_name_over_formula()`)
  with a Windows-style `figures_dir`; `figures_dir` is now also validated when
  the table is built rather than when it is first rendered (#35).

* Fixed spurious `ergm` version-compatibility warnings when tabulating a
  fitted model (#35).

* Fitted-model tables round estimates and standard errors to 2 decimal
  places by default and format p-values to match, showing values below the
  display precision as `<0.01`; control this with `digits` (#34, #37).

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
  fields; all shipped terms now carry curated text instead of falling
  back to the `ergm` database's (#24).

* Tables gained a `title` column (`include_title = TRUE`); `description`
  now holds prose instead of the short `ergm` title (#24).

* Added `override`/`override.*` arguments to `tabulergm_table()`,
  `parse_ergm_model()`, and `parse_ergm_formula()` for replacing any
  metadata field per term or per coefficient (#24).

* Cited terms now show a `(key)` marker with the matching reference listed
  below the table (#24).

* Fixed an incorrect arXiv identifier in `?"tabulergm-notation"`: 1412.1151
  was cited for Bomiriya et al. (2014) but belongs to an unrelated paper
  (#24).

## Internal changes

* Added CRAN, download, license, and dependency badges to the README.

* The please-bump check now also verifies that `NEWS.md` matches
  `DESCRIPTION`.

* Tests now drive the exported API instead of internal helpers, cutting the
  suite by about 500 lines with no loss of coverage (#38).


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
