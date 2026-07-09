

<!-- README.md is generated from README.qmd. Please edit that file -->

## tabulergm

<!-- badges: start -->

[![R-CMD-check](https://github.com/gvegayon/tabulergm/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/gvegayon/tabulergm/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

`tabulergm` is an R package designed to translate models estimated with
the [`ergm`](https://cran.r-project.org/package=ergm) framework into
publication-ready tables and explanatory summaries.

## Installation

``` r
# Install from GitHub
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
model_terms[, c("term", "estimate", "se", "pvalue", "description")]
#>       term   estimate        se       pvalue                    description
#> 1    edges -1.6507266 0.3179320 2.079634e-07 Number of edges in the network
#> 2 triangle  0.1082377 0.5183562 8.345969e-01                      Triangles
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

bipartite_terms[, c("term", "attribute", "description")]
#>          term attribute
#> 1     gwb1dsp      <NA>
#> 2     gwb2dsp      <NA>
#> 3    b1factor      type
#> 4    b2factor     group
#> 5 b1nodematch      type
#> 6 b2nodematch     group
#>                                                                                       description
#> 1  Geometrically weighted dyadwise shared partner distribution for dyads in the first bipartition
#> 2 Geometrically weighted dyadwise shared partner distribution for dyads in the second bipartition
#> 3                               Factor attribute effect for the first mode in a bipartite network
#> 4                              Factor attribute effect for the second mode in a bipartite network
#> 5                Nodal attribute-based homophily effect for the first mode in a bipartite network
#> 6               Nodal attribute-based homophily effect for the second mode in a bipartite network
```

We can also embed the table in quarto/Rmarkdown. The table below covers
every term currently included in `tabulergm`’s term dictionary; terms
with both directed and undirected definitions (`edges`, `gwesp`,
`gwdsp`) display the undirected version:

``` r
dictionary_terms <- network ~
  edges + mutual + triangle +
  gwesp(0.5, fixed = TRUE) + gwdsp(0.5, fixed = TRUE) +
  gwdegree(0.5, fixed = TRUE) +
  nodematch("attr") + nodefactor("attr") + nodemix("attr") +
  nodecov("attr") + absdiff("attr") + edgecov("cov") +
  transitiveties + cyclicalties +
  nodeicov("attr") + nodeocov("attr") +
  gwb1dsp(0.5, fixed = TRUE) + gwb2dsp(0.5, fixed = TRUE) +
  b1factor("type") + b2factor("group") +
  b1nodematch("type") + b2nodematch("group") +
  b1starmix(2, "type") + b2starmix(2, "group")

tabulergm_table(dictionary_terms, format = "markdown")
```

| term | figure | math | description |
|:---|:---|:---|:---|
| edges | <img src="man/figures/README-edges.png" width="80" /> | $\sum_{i<j} y_{ij}$ | Number of edges in the network |
| mutual | <img src="man/figures/README-mutual.png" width="80" /> | $\sum_{i<j} y_{ij} y_{ji}$ | Mutuality |
| triangle | <img src="man/figures/README-triangle.png" width="80" /> | $\sum_{i<j<k} y_{ij} y_{jk} y_{ik}$ | Triangles |
| gwesp | <img src="man/figures/README-gwesp.png" width="80" /> | $\exp{(\tau)} \sum_{i=1}^{n-2} \left[1 - \left(1 - \exp{(-\tau)}\right)^i\right] EP_i(y)$ | Geometrically weighted edgewise shared partner distribution |
| gwdsp | <img src="man/figures/README-gwdsp.png" width="80" /> | $\exp{(\tau)} \sum_{i=1}^{n-2} \left[1 - \left(1 - \exp{(-\tau)}\right)^i\right] DP_i(y)$ | Geometrically weighted dyadwise shared partner distribution |
| gwdegree | <img src="man/figures/README-gwdegree.png" width="80" /> | $\exp{(\tau)} \sum_{i=1}^{n-1} \left[1 - \left(1 - \exp{(-\tau)}\right)^i\right] D_i(y)$ | Geometrically weighted degree distribution |
| nodematch | <img src="man/figures/README-nodematch.png" width="80" /> | $\sum_{i<j} y_{ij} \mathbf{1}(x_i = x_j)$ | Uniform homophily and differential homophily |
| nodefactor | <img src="man/figures/README-nodefactor.png" width="80" /> | $\sum_{i<j} y_{ij} \left[\mathbf{1}(x_i = k) + \mathbf{1}(x_j = k)\right]$ | Factor attribute effect |
| nodemix | <img src="man/figures/README-nodemix.png" width="80" /> | $\sum_{i<j} y_{ij} \mathbf{1}(\{x_i, x_j\} = \{k, l\})$ | Nodal attribute mixing |
| nodecov | <img src="man/figures/README-nodecov.png" width="80" /> | $\sum_{i<j} y_{ij} (x_i + x_j)$ | Main effect of a covariate |
| absdiff | <img src="man/figures/README-absdiff.png" width="80" /> | $\sum_{i<j} y_{ij} \left&#124;x_i - x_j\right&#124;$ | Absolute difference in nodal attribute |
| edgecov | <img src="man/figures/README-edgecov.png" width="80" /> | $\sum_{i<j} y_{ij} x_{ij}$ | Edge covariate |
| transitiveties | <img src="man/figures/README-transitiveties.png" width="80" /> | $\sum_{i \neq j} y_{ij} \mathbf{1}\left(\exists k : y_{ik} y_{kj} = 1\right)$ | Transitive ties |
| cyclicalties | <img src="man/figures/README-cyclicalties.png" width="80" /> | $\sum_{i \neq j} y_{ij} \mathbf{1}\left(\exists k : y_{jk} y_{ki} = 1\right)$ | Cyclical ties |
| nodeicov | <img src="man/figures/README-nodeicov.png" width="80" /> | $\sum_{i \neq j} y_{ij} x_j$ | Main effect of a covariate for in-edges |
| nodeocov | <img src="man/figures/README-nodeocov.png" width="80" /> | $\sum_{i \neq j} y_{ij} x_i$ | Main effect of a covariate for out-edges |
| gwb1dsp | <img src="man/figures/README-gwb1dsp.png" width="80" /> | $\exp{(\tau)} \sum_{i=1}^{n-2} \left[1 - \left(1 - \exp{(-\tau)}\right)^m\right] DP_i(y)$ | Geometrically weighted dyadwise shared partner distribution for dyads in the first bipartition |
| gwb2dsp | <img src="man/figures/README-gwb2dsp.png" width="80" /> | $\exp{(\tau)} \sum_{i=1}^{n-2} \left[1 - \left(1 - \exp{(-\tau)}\right)^m\right] DP_i(y)$ | Geometrically weighted dyadwise shared partner distribution for dyads in the second bipartition |
| b1factor | <img src="man/figures/README-b1factor.png" width="80" /> | $\sum_{i \in B_1} \sum_{j \in B_2} y_{ij} \mathbf{1}(x_i = k)$ | Factor attribute effect for the first mode in a bipartite network |
| b2factor | <img src="man/figures/README-b2factor.png" width="80" /> | $\sum_{i \in B_1} \sum_{j \in B_2} y_{ij} \mathbf{1}(x_j = k)$ | Factor attribute effect for the second mode in a bipartite network |
| b1nodematch | <img src="man/figures/README-b1nodematch.png" width="80" /> | $\sum_{k\in B_1} \sum_{i<j \in B_2} \mathbf{1}(x_i = x_j) y_{ik} y_{jk}$ | Nodal attribute-based homophily effect for the first mode in a bipartite network |
| b2nodematch | <img src="man/figures/README-b2nodematch.png" width="80" /> | $\sum_{k\in B_2} \sum_{i<j \in B_1} \mathbf{1}(x_i = x_j) y_{ik} y_{jk}$ | Nodal attribute-based homophily effect for the second mode in a bipartite network |
| b1starmix | <img src="man/figures/README-b1starmix.png" width="80" /> | $\sum_{i \in B_1} \mathbf{1}(x_i = p) \sum_{j_1 < \cdots < j_k \in B_2} \prod_{l=1}^{k} y_{i j_l} \mathbf{1}(x_{j_l} = q)$ | Mixing matrix for k-stars centered on the first mode of a bipartite network |
| b2starmix | <img src="man/figures/README-b2starmix.png" width="80" /> | $\sum_{j \in B_2} \mathbf{1}(x_j = p) \sum_{i_1 < \cdots < i_k \in B_1} \prod_{l=1}^{k} y_{i_l j} \mathbf{1}(x_{i_l} = q)$ | Mixing matrix for k-stars centered on the second mode of a bipartite network |

## Code of Conduct

Please note that the tabulergm project is released with a [Contributor
Code of
Conduct](https://gvegayon.github.io/tabulergm/CODE_OF_CONDUCT.html). By
contributing to this project, you agree to abide by its terms.
