

<!-- README.md is generated from README.qmd. Please edit that file -->

## tabulergm <img src="man/figures/logo.png" align="right" height="200" alt="tabulergm hex sticker logo"/>

<!-- badges: start -->

[![CRAN
status](https://www.r-pkg.org/badges/version/tabulergm)](https://CRAN.R-project.org/package=tabulergm)
[![R-CMD-check](https://github.com/gvegayon/tabulergm/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/gvegayon/tabulergm/actions/workflows/R-CMD-check.yaml)
[![CRANlogs
downloads](https://cranlogs.r-pkg.org/badges/grand-total/tabulergm)](https://cran.r-project.org/package=tabulergm)
[![License:
MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://github.com/gvegayon/tabulergm/blob/main/LICENSE.md)
[![status](https://tinyverse.netlify.app/badge/tabulergm)](https://CRAN.R-project.org/package=tabulergm)
<!-- badges: end -->

The `tabulergm` package provides an easy way to generate
publication-ready tables and explanatory summaries for
Exponential-Random Graph Family Models \[ERGMs\]. Users can generate
tables either directly from fitted models or by parsing a formula to
inspect the available terms and their metadata. Generated tables can be
incorporated into Quarto or RMarkdown documents, and can also be
exported as Markdown or LaTeX snippets for use in other projects.

The package includes a term dictionary with metadata for each term: a
short title, a plain-language description, a LaTeX definition, an
example figure, and – where the term has an identifiable source – a
citation. Figures are currently drawn using the
[`netplot`](https://cran.r-project.org/package=netplot) package, but
users can also provide their own custom figure generation methods. Every
text field can be replaced on a per-table basis without editing the
dictionary (see [Customizing term
metadata](#customizing-term-metadata)).

Future version of this package may extend its functionality to support
stochastic actor-oriented models (SAOMs) and other network modeling
frameworks.

## Installation

``` r
# From CRAN
install.packages("tabulergm")

# Development version from GitHub
# install.packages("remotes")
remotes::install_github("gvegayon/tabulergm")
```

## Example

Here is a simple example fitting an ERGM to the Florentine marriage
network and generating a summary table:

``` r
library(ergm)
library(tabulergm)

# Fit a simple ERGM
data(florentine)
model <- ergm(
  flomarriage ~ edges + triangle,
  control = control.ergm(seed = 42)
)

# Parse the model
model_terms <- parse_ergm_model(model)
model_terms[, c("term", "title", "estimate", "se", "pvalue")]
#>       term           title   estimate        se       pvalue
#> 1    edges Number of edges -1.6507266 0.3179320 2.079634e-07
#> 2 triangle       Triangles  0.1082377 0.5183562 8.345969e-01
```

You can also export the table code and generated term figures into a
folder that can be copied into another paper or report project:

``` r
tabulergm_save(
  model,
  "exports/florentine-ergm",
  include_math = TRUE
)
```

This writes Markdown and LaTeX table snippets plus a `figures/` folder
with the copied image assets.

The term dictionary also includes mode-specific terms for bipartite
ERGMs. A formula is enough to inspect the available metadata before
fitting a model:

``` r
bipartite_terms <- parse_ergm_formula(
  network ~
    gwb1dsp(0.5, fixed = TRUE) + gwb2dsp(0.5, fixed = TRUE) +
    b1factor("type") + b2factor("group") +
    b1nodematch("type") + b2nodematch("group")
)

bipartite_terms[, c("term", "attribute", "title")]
#>          term attribute
#> 1     gwb1dsp      <NA>
#> 2     gwb2dsp      <NA>
#> 3    b1factor      type
#> 4    b2factor     group
#> 5 b1nodematch      type
#> 6 b2nodematch     group
#>                                                           title
#> 1  Geometrically weighted dyadwise shared partners (first mode)
#> 2 Geometrically weighted dyadwise shared partners (second mode)
#> 3                                 First-mode attribute activity
#> 4                                Second-mode attribute activity
#> 5                                          First-mode homophily
#> 6                                         Second-mode homophily
```

## Customizing term metadata

Titles and descriptions come from the term dictionary, falling back to
the `ergm` term database for terms the dictionary does not cover. Either
can be replaced for a single table with the `override.*` arguments,
which are keyed by term name:

``` r
custom <- tabulergm_table(
  model,
  include_title  = TRUE,
  override.title = c(edges = "Density"),
  override.desc  = c(edges = "Baseline propensity to form ties.")
)

custom[, c("term", "title")]
#>                   term     title
#> 1  edges (holland1981)   Density
#> 2 triangle (frank1986) Triangles
```

`edges` and `triangle` pick up `(holland1981)` and `(frank1986)` markers
because they carry citations in the term dictionary. The marker attaches
to the description when that column is shown and to the term otherwise,
so a citation is never silently dropped.

The same works for `math`, `figure`, and `citation`, and a single
`override` list can set several fields of several terms at once:

``` r
tabulergm_table(
  model,
  override = list(
    edges     = list(title = "Density", desc = "Baseline tie propensity."),
    nodematch = list(citation = "doi:10.1016/S0378-8733(01)00029-6")
  )
)
```

Terms that carry a citation get a `(key)` marker next to their
description, and the matching reference is appended below the table.
Storing a DOI, arXiv id, or PubMed id (rather than a formatted
reference) lets readers pull the full citation into their own
bibliography software:

``` r
tabulergm_table(
  network ~ gwesp(0.5, fixed = TRUE) + triangle,
  format = "markdown"
)
```

| term | figure | math | description |
|:---|:---|:---|:---|
| gwesp | <img src="man/figures/README-gwesp.png" width="80" /> | $\exp{(\tau)} \sum_{i=1}^{n-2} \left[1 - \left(1 - \exp{(-\tau)}\right)^i\right] EP_i(y)$ | Summarizes how many partners tied nodes share, weighting each additional shared partner geometrically less than the last. Provides a better-behaved measure of transitive closure than a raw triangle count; the decay parameter controls how fast the weights fall off. (snijders2006; hunter2007) |
| triangle | <img src="man/figures/README-triangle.png" width="80" /> | $\sum_{i<j<k} y_{ij} y_{jk} y_{ik}$ | Counts the sets of three mutually connected nodes, the basic measure of local clustering in an undirected network. (frank1986) |

*\[snijders2006\]
[doi:10.1111/j.1467-9531.2006.00176.x](https://doi.org/10.1111/j.1467-9531.2006.00176.x)*\
*\[hunter2007\]
[doi:10.1016/j.socnet.2006.08.005](https://doi.org/10.1016/j.socnet.2006.08.005)*\
*\[frank1986\]
[doi:10.1080/01621459.1986.10478342](https://doi.org/10.1080/01621459.1986.10478342)*

## Table styles

The default layout keeps one metadata component per column. For a
compact term dictionary, `with_style_name_over_formula()` stacks each
term title over its formula and moves the figure into a
**Representation** column. It retains model estimates and any metadata
columns you explicitly include:

``` r
tabulergm_table(
  model,
  include_description = FALSE,
  format = "markdown"
) |>
  with_style_name_over_formula()
```

<table class="tabulergm-table tabulergm-style-name-over-formula"
data-quarto-postprocess="true">
<colgroup>
<col style="width: 20%" />
<col style="width: 20%" />
<col style="width: 20%" />
<col style="width: 20%" />
<col style="width: 20%" />
</colgroup>
<thead>
<tr>
<th style="text-align: left;" data-quarto-table-cell-role="th">Name</th>
<th style="text-align: left;"
data-quarto-table-cell-role="th">Representation</th>
<th style="text-align: right;"
data-quarto-table-cell-role="th">estimate</th>
<th style="text-align: right;" data-quarto-table-cell-role="th">se</th>
<th style="text-align: right;"
data-quarto-table-cell-role="th">pvalue</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align: left;">Number of edges (holland1981)<br />
<span class="tabulergm-formula">$$\sum_{i\lt{}j} y_{ij}$$</span></td>
<td class="tabulergm-representation" style="text-align: center;"><img
src="man/figures/README-edges.png" style="width:40%;max-width:100%;"
alt="term figure" /></td>
<td style="text-align: right;">-1.65</td>
<td style="text-align: right;">0.32</td>
<td style="text-align: right;">&lt;0.01</td>
</tr>
<tr>
<td style="text-align: left;">Triangles (frank1986)<br />
<span class="tabulergm-formula">$$\sum_{i\lt{}j\lt{}k} y_{ij} y_{jk}
y_{ik}$$</span></td>
<td class="tabulergm-representation" style="text-align: center;"><img
src="man/figures/README-triangle.png" style="width:40%;max-width:100%;"
alt="term figure" /></td>
<td style="text-align: right;">0.11</td>
<td style="text-align: right;">0.52</td>
<td style="text-align: right;">0.83</td>
</tr>
</tbody>
</table>

<p class="tabulergm-citations"><em>[holland1981] <a href="https://doi.org/10.1080/01621459.1981.10477598">doi:10.1080/01621459.1981.10477598</a></em><br><em>[frank1986] <a href="https://doi.org/10.1080/01621459.1986.10478342">doi:10.1080/01621459.1986.10478342</a></em></p>

Styled Markdown uses an HTML table so multi-line name/formula cells
render reliably in HTML-capable Markdown outputs. Use the plain style
for a portable pipe table in non-HTML targets. `with_style_plain()`
restores the original layout at any point in a pipeline.

Styled tables can also be saved or previewed directly:

``` r
tabulergm_table(model, include_description = FALSE) |>
  with_style_name_over_formula() |>
  tabulergm_save("exports/compact-ergm")

tabulergm_table(model, include_description = FALSE) |>
  with_style_name_over_formula() |>
  tabulergm_view()
```

The compact LaTeX export uses `array`, `booktabs`, and `graphicx` for
its multiline cells and figures.

Fitted-model tables round every numeric column (estimates, standard
errors, and p-values) to two decimal places by default while retaining
full-precision values in the attached table specification. Set
`digits = NULL` to display full precision, or choose a different number
of decimal places:

``` r
tabulergm_table(model, digits = 3)
```

Compact-style presentation settings are also retained by the table
object, so they survive previews and exports. Column widths are
fractions of the table width and figure height is measured in inches;
omit either setting for automatic columns and the default width-based
figure size:

``` r
tabulergm_table(model, include_description = FALSE) |>
  with_style_name_over_formula(
    column_widths = c(Name = .5, Representation = .2),
    figure_height = .8
  ) |>
  tabulergm_save("exports/compact-ergm")
```

## The term dictionary

We can also embed the table in quarto/Rmarkdown. The table below covers
every term currently included in `tabulergm`’s term dictionary; terms
with both directed and undirected definitions (`edges`, `gwesp`,
`gwdsp`, `isolates`) display the undirected version:

``` r
dictionary_terms <- network ~
  edges + mutual + triangle +
  gwesp(0.5, fixed = TRUE) + gwdsp(0.5, fixed = TRUE) +
  gwdegree(0.5, fixed = TRUE) + altkstar(2, fixed = TRUE) +
  nodematch("attr") + nodefactor("attr") + nodemix("attr") +
  nodecov("attr") + absdiff("attr") + edgecov("cov") +
  transitiveties + cyclicalties +
  nodeicov("attr") + nodeocov("attr") +
  gwidegree(0.5, fixed = TRUE) + gwodegree(0.5, fixed = TRUE) +
  nodeifactor("attr") + nodeofactor("attr") +
  kstar(2) + istar(2) + ostar(2) +
  isolates + degree(1) + concurrent +
  dgwesp(0.5, fixed = TRUE) + dgwdsp(0.5, fixed = TRUE) +
  gwb1dsp(0.5, fixed = TRUE) + gwb2dsp(0.5, fixed = TRUE) +
  gwb1degree(0.5, fixed = TRUE) + gwb2degree(0.5, fixed = TRUE) +
  b1factor("type") + b2factor("group") +
  b1nodematch("type") + b2nodematch("group") +
  b1starmix(2, "type") + b2starmix(2, "group")

tabulergm_table(dictionary_terms, format = "markdown")
```

| term | figure | math | description |
|:---|:---|:---|:---|
| edges | <img src="man/figures/README-edges.png" width="80" /> | $\sum_{i<j} y_{ij}$ | Counts the ties present in the network. Acts as the baseline density term, playing the role an intercept plays in a regression model. (holland1981) |
| mutual | <img src="man/figures/README-mutual.png" width="80" /> | $\sum_{i<j} y_{ij} y_{ji}$ | Counts the dyads in which both directed ties are present, capturing the tendency for ties to be returned. (holland1981) |
| triangle | <img src="man/figures/README-triangle.png" width="80" /> | $\sum_{i<j<k} y_{ij} y_{jk} y_{ik}$ | Counts the sets of three mutually connected nodes, the basic measure of local clustering in an undirected network. (frank1986) |
| gwesp | <img src="man/figures/README-gwesp.png" width="80" /> | $\exp{(\tau)} \sum_{i=1}^{n-2} \left[1 - \left(1 - \exp{(-\tau)}\right)^i\right] EP_i(y)$ | Summarizes how many partners tied nodes share, weighting each additional shared partner geometrically less than the last. Provides a better-behaved measure of transitive closure than a raw triangle count; the decay parameter controls how fast the weights fall off. (snijders2006; hunter2007) |
| gwdsp | <img src="man/figures/README-gwdsp.png" width="80" /> | $\exp{(\tau)} \sum_{i=1}^{n-2} \left[1 - \left(1 - \exp{(-\tau)}\right)^i\right] DP_i(y)$ | Summarizes shared partners over every dyad, tied or not, with geometrically decreasing weights. Commonly paired with gwesp to separate shared partnership from closure itself. (snijders2006; hunter2007) |
| gwdegree | <img src="man/figures/README-gwdegree.png" width="80" /> | $\exp{(\tau)} \sum_{i=1}^{n-1} \left[1 - \left(1 - \exp{(-\tau)}\right)^i\right] D_i(y)$ | Summarizes the degree distribution with geometrically decreasing weights. Captures whether ties concentrate on a few high-degree nodes or spread evenly, and stabilizes models that would otherwise degenerate. (snijders2006; hunter2007) |
| altkstar | <img src="man/figures/README-altkstar.png" width="80" /> | $\sum_{k=2}^{n-1} (-1)^k \frac{S_k(y)}{\lambda^{k-2}}$ | Alternating sum of the k-star counts, an equivalent parameterization of the geometrically weighted degree distribution used to model degree heterogeneity. (snijders2006; hunter2007) |
| nodematch | <img src="man/figures/README-nodematch.png" width="80" /> | $\sum_{i<j} y_{ij} \mathbf{1}(x_i = x_j)$ | Counts the ties joining nodes that share the same value of a categorical attribute, the standard measure of homophily. Setting diff = TRUE adds one statistic per attribute value (differential homophily). (wasserman1996; mcpherson2001) |
| nodefactor | <img src="man/figures/README-nodefactor.png" width="80" /> | $\sum_{i<j} y_{ij} \left[\mathbf{1}(x_i = k) + \mathbf{1}(x_j = k)\right]$ | Counts the tie endpoints belonging to each level of a categorical attribute, measuring how active nodes with that value are in forming ties. |
| nodemix | <img src="man/figures/README-nodemix.png" width="80" /> | $\sum_{i<j} y_{ij} \mathbf{1}(\{x_i, x_j\} = \{k, l\})$ | Counts the ties for every pairing of attribute values, reproducing the full mixing matrix of a categorical attribute. (wasserman1996; morris1991) |
| nodecov | <img src="man/figures/README-nodecov.png" width="80" /> | $\sum_{i<j} y_{ij} (x_i + x_j)$ | Sums a quantitative attribute over both ends of each tie, measuring how strongly that attribute drives tie formation. |
| absdiff | <img src="man/figures/README-absdiff.png" width="80" /> | $\sum_{i<j} y_{ij} \left\lvert{}x_i - x_j\right\rvert{}$ | Sums the absolute difference in a quantitative attribute across tied nodes. Negative estimates indicate homophily, since similar nodes contribute less. |
| edgecov | <img src="man/figures/README-edgecov.png" width="80" /> | $\sum_{i<j} y_{ij} x_{ij}$ | Sums a fixed dyad-level covariate over the observed ties, letting an external matrix such as distance or a previously observed network predict tie formation. (wasserman1996) |
| transitiveties | <img src="man/figures/README-transitiveties.png" width="80" /> | $\sum_{i \neq j} y_{ij} \mathbf{1}\left(\exists k : y_{ik} y_{kj} = 1\right)$ | Counts the ties closed by at least one two-path. Unlike a triple count, a tie contributes once no matter how many shared partners it has. |
| cyclicalties | <img src="man/figures/README-cyclicalties.png" width="80" /> | $\sum_{i \neq j} y_{ij} \mathbf{1}\left(\exists k : y_{jk} y_{ki} = 1\right)$ | Counts the ties that take part in at least one cycle, capturing generalized exchange rather than hierarchy. |
| nodeicov | <img src="man/figures/README-nodeicov.png" width="80" /> | $\sum_{i \neq j} y_{ij} x_j$ | Sums the receiving node’s attribute value over all ties, measuring how a quantitative attribute drives incoming ties (popularity). |
| nodeocov | <img src="man/figures/README-nodeocov.png" width="80" /> | $\sum_{i \neq j} y_{ij} x_i$ | Sums the sending node’s attribute value over all ties, measuring how a quantitative attribute drives outgoing ties (activity). |
| gwidegree | <img src="man/figures/README-gwidegree.png" width="80" /> | $\exp{(\tau)} \sum_{i=1}^{n-1} \left[1 - \left(1 - \exp{(-\tau)}\right)^i\right] D^{\mathrm{in}}_i(y)$ | Summarizes the in-degree distribution with geometrically decreasing weights. Captures whether incoming ties concentrate on a few popular nodes or spread evenly across receivers. (hunter2007; robins2009) |
| gwodegree | <img src="man/figures/README-gwodegree.png" width="80" /> | $\exp{(\tau)} \sum_{i=1}^{n-1} \left[1 - \left(1 - \exp{(-\tau)}\right)^i\right] D^{\mathrm{out}}_i(y)$ | Summarizes the out-degree distribution with geometrically decreasing weights. Captures whether outgoing ties concentrate on a few highly active nodes or spread evenly across senders. (hunter2007; robins2009) |
| nodeifactor | <img src="man/figures/README-nodeicov.png" width="80" /> | $\sum_{i \neq j} y_{ij} \mathbf{1}(x_j = k)$ | Counts the incoming ties received by nodes at each level of a categorical attribute, measuring how popular nodes with that value are as receivers. |
| nodeofactor | <img src="man/figures/README-nodeocov.png" width="80" /> | $\sum_{i \neq j} y_{ij} \mathbf{1}(x_i = k)$ | Counts the outgoing ties sent by nodes at each level of a categorical attribute, measuring how active nodes with that value are as senders. |
| kstar | <img src="man/figures/README-kstar.png" width="80" /> | $\sum_{i} \binom{\sum_{j \neq i} y_{ij}}{k}$ | Counts the sets of k ties that share a common node, a Markov dependence measure of degree heterogeneity. Pass several values of k to include one statistic per star size. (frank1986) |
| istar | <img src="man/figures/README-istar.png" width="80" /> | $\sum_{j} \binom{\sum_{i \neq j} y_{ij}}{k}$ | Counts the sets of k incoming ties that share a common receiver, capturing the spread of in-degrees (popularity). Pass several values of k to include one statistic per star size. (wasserman1996) |
| ostar | <img src="man/figures/README-ostar.png" width="80" /> | $\sum_{i} \binom{\sum_{j \neq i} y_{ij}}{k}$ | Counts the sets of k outgoing ties that share a common sender, capturing the spread of out-degrees (activity). Pass several values of k to include one statistic per star size. (wasserman1996) |
| isolates | <img src="man/figures/README-isolates.png" width="80" /> | $\sum_{i} \mathbf{1}\left(\sum_{j \neq i} y_{ij} = 0\right)$ | Counts the nodes with no ties, capturing an excess (or shortage) of isolated nodes relative to the rest of the degree distribution. |
| degree | <img src="man/figures/README-degree.png" width="80" /> | $\sum_{i} \mathbf{1}\left(\sum_{j \neq i} y_{ij} = d\right)$ | Counts the nodes with exactly d ties. Pass several values of d to include one statistic per degree, e.g. to model low-degree nodes explicitly. |
| concurrent | <img src="man/figures/README-gwdegree.png" width="80" /> | $\sum_{i} \mathbf{1}\left(\sum_{j \neq i} y_{ij} \geq 2\right)$ | Counts the nodes with two or more ties, the number of actors holding concurrent partnerships. Common in models of sexual networks and disease transmission. |
| dgwesp | <img src="man/figures/README-dgwesp.png" width="80" /> | $\exp{(\tau)} \sum_{i=1}^{n-2} \left[1 - \left(1 - \exp{(-\tau)}\right)^i\right] EP^{\mathrm{OTP}}_i(y)$ | Directed counterpart of the edgewise shared partner statistic, measuring transitive closure with geometrically decreasing weights. Outgoing two-paths are counted by default; the term’s type argument selects a different two-path orientation. (hunter2007; robins2009) |
| dgwdsp | <img src="man/figures/README-dgwdsp.png" width="80" /> | $\exp{(\tau)} \sum_{i=1}^{n-2} \left[1 - \left(1 - \exp{(-\tau)}\right)^i\right] DP^{\mathrm{OTP}}_i(y)$ | Directed counterpart of the dyadwise shared partner statistic, computed over every ordered dyad. Outgoing two-paths are counted by default; the term’s type argument selects a different two-path orientation. (hunter2007; robins2009) |
| gwb1dsp | <img src="man/figures/README-gwb1dsp.png" width="80" /> | $\exp{(\tau)} \sum_{i=1}^{n_{B_2}} \left[1 - \left(1 - \exp{(-\tau)}\right)^i\right] DP_i(y)$ | Summarizes how many second-mode nodes each pair of first-mode nodes has in common, weighting additional shared partners geometrically less. (wang2009; hunter2007) |
| gwb2dsp | <img src="man/figures/README-gwb2dsp.png" width="80" /> | $\exp{(\tau)} \sum_{i=1}^{n_{B_1}} \left[1 - \left(1 - \exp{(-\tau)}\right)^i\right] DP_i(y)$ | Summarizes how many first-mode nodes each pair of second-mode nodes has in common, weighting additional shared partners geometrically less. (wang2009; hunter2007) |
| gwb1degree | <img src="man/figures/README-gwb1degree.png" width="80" /> | $\exp{(\tau)} \sum_{i=1}^{n_{B_2}} \left[1 - \left(1 - \exp{(-\tau)}\right)^i\right] D^{B_1}_i(y)$ | Summarizes the degree distribution of first-mode nodes with geometrically decreasing weights. Captures whether ties to the second mode concentrate on a few highly active first-mode nodes or spread evenly across them. (wang2009; hunter2007) |
| gwb2degree | <img src="man/figures/README-gwb2degree.png" width="80" /> | $\exp{(\tau)} \sum_{i=1}^{n_{B_1}} \left[1 - \left(1 - \exp{(-\tau)}\right)^i\right] D^{B_2}_i(y)$ | Summarizes the degree distribution of second-mode nodes with geometrically decreasing weights. Captures whether ties from the first mode concentrate on a few popular second-mode nodes or spread evenly across them. (wang2009; hunter2007) |
| b1factor | <img src="man/figures/README-b1factor.png" width="80" /> | $\sum_{i \in B_1} \sum_{j \in B_2} y_{ij} \mathbf{1}(x_i = k)$ | Counts the ties incident on first-mode nodes at each level of a categorical attribute, measuring how active those nodes are. |
| b2factor | <img src="man/figures/README-b2factor.png" width="80" /> | $\sum_{i \in B_1} \sum_{j \in B_2} y_{ij} \mathbf{1}(x_j = k)$ | Counts the ties incident on second-mode nodes at each level of a categorical attribute, measuring how active those nodes are. |
| b1nodematch | <img src="man/figures/README-b1nodematch.png" width="80" /> | $\sum_{k\in B_2} \sum_{i<j \in B_1} \mathbf{1}(x_i = x_j) y_{ik} y_{jk}$ | Counts the pairs of first-mode nodes that share an attribute value and are both tied to the same second-mode node. The alpha and beta discount parameters temper the count when nodes share many partners. (bomiriya2023) |
| b2nodematch | <img src="man/figures/README-b2nodematch.png" width="80" /> | $\sum_{k\in B_1} \sum_{i<j \in B_2} \mathbf{1}(x_i = x_j) y_{ik} y_{jk}$ | Counts the pairs of second-mode nodes that share an attribute value and are both tied to the same first-mode node. The alpha and beta discount parameters temper the count when nodes share many partners. (bomiriya2023) |
| b1starmix | <img src="man/figures/README-b1starmix.png" width="80" /> | $\sum_{i \in B_1} \mathbf{1}(x_i = p) \sum_{j_1 < \cdots < j_k \in B_2} \prod_{l=1}^{k} y_{i j_l} \mathbf{1}(x_{j_l} = q)$ | Counts the k-stars centered on a first-mode node with one attribute value whose second-mode neighbors all share another, capturing mixing and degree together. |
| b2starmix | <img src="man/figures/README-b2starmix.png" width="80" /> | $\sum_{j \in B_2} \mathbf{1}(x_j = p) \sum_{i_1 < \cdots < i_k \in B_1} \prod_{l=1}^{k} y_{i_l j} \mathbf{1}(x_{i_l} = q)$ | Counts the k-stars centered on a second-mode node with one attribute value whose first-mode neighbors all share another, capturing mixing and degree together. |

*Note: Orange nodes indicate nodes with a focal attribute. Orange and
teal nodes represent nodes with different values of the focal attribute.
Square nodes represent nodes in the first mode and circle nodes in the
second mode.*

*\[holland1981\]
[doi:10.1080/01621459.1981.10477598](https://doi.org/10.1080/01621459.1981.10477598)*\
*\[frank1986\]
[doi:10.1080/01621459.1986.10478342](https://doi.org/10.1080/01621459.1986.10478342)*\
*\[snijders2006\]
[doi:10.1111/j.1467-9531.2006.00176.x](https://doi.org/10.1111/j.1467-9531.2006.00176.x)*\
*\[hunter2007\]
[doi:10.1016/j.socnet.2006.08.005](https://doi.org/10.1016/j.socnet.2006.08.005)*\
*\[wasserman1996\]
[doi:10.1007/BF02294547](https://doi.org/10.1007/BF02294547)*\
*\[mcpherson2001\]
[doi:10.1146/annurev.soc.27.1.415](https://doi.org/10.1146/annurev.soc.27.1.415)*\
*\[morris1991\]
[doi:10.1016/0025-5564(91)90014-A](https://doi.org/10.1016/0025-5564(91)90014-A)*\
*\[robins2009\]
[doi:10.1016/j.socnet.2008.10.006](https://doi.org/10.1016/j.socnet.2008.10.006)*\
*\[wang2009\]
[doi:10.1016/j.socnet.2008.08.002](https://doi.org/10.1016/j.socnet.2008.08.002)*\
*\[bomiriya2023\] [arXiv:2312.05673](https://arxiv.org/abs/2312.05673)*

## Code of Conduct

Please note that the tabulergm project is released with a [Contributor
Code of
Conduct](https://gvegayon.github.io/tabulergm/CODE_OF_CONDUCT.html). By
contributing to this project, you agree to abide by its terms.
