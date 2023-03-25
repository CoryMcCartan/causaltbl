
<!-- README.md is generated from README.Rmd. Please edit that file -->

# causaltbl

<!-- badges: start -->

[![R-CMD-check](https://github.com/CoryMcCartan/causaltbl/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/CoryMcCartan/causaltbl/actions/workflows/R-CMD-check.yaml)
[![codecov](https://codecov.io/github/CoryMcCartan/causaltbl/branch/main/graph/badge.svg?token=TXL7DJG9U9)](https://codecov.io/github/CoryMcCartan/causaltbl)
<!-- badges: end -->

This package provides a `causal_tbl` class for causal inference. A
`causal_tbl` is a subclass of `tibble` which keeps track of information
on the roles of variables like treatment and outcome, and provides
functionality to store models and their fitted values as columns in a
data frame.

## Installation

You can install the development version of causaltbl from
[GitHub](https://github.com/) with:

``` r
# install.packages("remotes")
remotes::install_github("CoryMcCartan/causaltbl")
```
