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
