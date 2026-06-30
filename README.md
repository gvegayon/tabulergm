

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

## License

MIT © George Vega Yon
