

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
model <- ergm(flomarriage ~ edges + triangle)

# Parse the model
parse_ergm_model(model)
#>       term coef_name attribute   estimate        se       pvalue
#> 1    edges     edges      <NA> -1.7013348 0.3697348 4.194593e-06
#> 2 triangle  triangle      <NA>  0.1957482 0.5572630 7.253879e-01
#>                      description                               math
#> 1 Number of edges in the network                 \\sum_{i<j} y_{ij}
#> 2                      Triangles \\sum_{i<j<k} y_{ij} y_{jk} y_{ik}
#>                                                 figure
#> 1 /tmp/RtmpQUrimx/ee181bc8e0ec4d7031265399c458b232.png
#> 2 /tmp/RtmpQUrimx/058b92bf247dcf2024909eb22259b055.png
```

## License

MIT © George Vega Yon
