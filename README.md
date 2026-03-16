

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
#>       term coef_name attribute   estimate        se       pvalue description
#> 1    edges     edges      <NA> -1.6982485 0.3458766 9.108368e-07        <NA>
#> 2 triangle  triangle      <NA>  0.1922093 0.5603898 7.316043e-01        <NA>
#>   math figure
#> 1 <NA>   <NA>
#> 2 <NA>   <NA>
```

## License

MIT © George Vega Yon
