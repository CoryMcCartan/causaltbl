
<!-- README.md is generated from README.Rmd. Please edit that file -->

# causaltbl <img src="man/figures/logo.png" align="right" height="173" />

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

## Using `causaltbl`

A causal tibble, `causal_tbl`, is a data frame with attributes
identifying which columns correspond to common inputs in causal
inference analyses. At the most basic level, you can indicate the
outcome and treatment columns. For more involved analyses, `causal_tbl`s
can keep track of additional columns including multiple outcomes and
multiple treatments.

The primary entryway to `causaltbl` is through
<!--- [`tidycausal`](https://corymccartan.com/tidycausal/) -->. You can
create a `causal_tbl` directly via `causal_tbl()`.

Suppose we have data from a really simple differences in differences
design. Our data looks like this:

``` r
df <- data.frame(
  id = c("a", "a", "a", "a", "b", "b", "b", "b"),
  year = rep(2015:2018, 2),
  trt = c(0, 0, 0, 0, 0, 0, 1, 1),
  y = c(1, 3, 2, 3, 2, 4, 4, 5)
)
```

There are two units (`id`), `a` and `b`. We have 4 yearly observations
from 2015 to 2018 (`year`) for each unit. `a` is never treated and `b`
is treated in 2017 and 2018 (`trt`). Some outcome (`y`) is measured
yearly.

We first can make a `causal_tbl` by passing `df` to `causal_tbl()`. We
don’t need to specify any options.

``` r
library(causaltbl)
did <- causal_tbl(df)
```

Now `did` is a `causal_tbl` version of `df`.

``` r
did
#> # A <causal_tbl> [8 × 4]
#>                          
#>   id     year   trt     y
#>   <chr> <int> <dbl> <dbl>
#> 1 a      2015     0     1
#> 2 a      2016     0     3
#> 3 a      2017     0     2
#> 4 a      2018     0     3
#> 5 b      2015     0     2
#> 6 b      2016     0     4
#> 7 b      2017     1     4
#> 8 b      2018     1     5
```

To set outcome , we can use the corresponding functions `set_outcome()`.
`causal_tbl` uses tidy evaluation, so we can use the bare column name.

``` r
did <- did |>
    set_outcome(outcome = y)
did
#> # A <causal_tbl> [8 × 4]
#>                     [out]
#>   id     year   trt     y
#>   <chr> <int> <dbl> <dbl>
#> 1 a      2015     0     1
#> 2 a      2016     0     3
#> 3 a      2017     0     2
#> 4 a      2018     0     3
#> 5 b      2015     0     2
#> 6 b      2016     0     4
#> 7 b      2017     1     4
#> 8 b      2018     1     5
```

Similarly, we can indicate that `did` has a treatment column `trt` or
panel structure for each `id`-`year` with the corresponding
`set_treatment()` and `set_panel()` functions.

``` r
did <- did |>
    set_treatment(treatment = trt) |>
    set_panel(unit = id, time = year)
did
#> # A <causal_tbl> [8 × 4]
#>   [unit] [time] [trt] [out]
#>   id       year   trt     y
#>   <chr>   <int> <dbl> <dbl>
#> 1 a        2015     0     1
#> 2 a        2016     0     3
#> 3 a        2017     0     2
#> 4 a        2018     0     3
#> 5 b        2015     0     2
#> 6 b        2016     0     4
#> 7 b        2017     1     4
#> 8 b        2018     1     5
```

This sets attributes that are used down-the-line by other packages. We
can retrieve them by calling their `get`ters. For the outcome,
`get_outcome()`:

``` r
get_outcome(did)
#> [1] "y"
```

For the treatment, `get_treatment()`:

``` r
get_treatment(did)
#>     y 
#> "trt"
```

And for the panel structure, `get_panel()`:

``` r
get_panel(did)
#> $unit
#> [1] "id"
#> 
#> $time
#> [1] "year"
```

For more information on using `causal_tbl`s or designing functions that
use `causal_tbl`s, see the Advanced `causal_tbl` vignette.
