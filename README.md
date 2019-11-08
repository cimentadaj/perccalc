
# perccalc <a href='https://cimentadaj.github.io/perccalc/'><img src='man/figures/logo/logo_hex.png' align="right" height="150" /></a>

[![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/perccalc)](http://cran.r-project.org/package=perccalc)
[![Travis-CI Build
Status](https://travis-ci.org/cimentadaj/perccalc.svg?branch=master)](https://travis-ci.org/cimentadaj/perccalc)
[![AppVeyor build
status](https://ci.appveyor.com/api/projects/status/github/cimentadaj/perccalc?branch=master&svg=true)](https://ci.appveyor.com/project/cimentadaj/perccalc)
[![Codecov test
coverage](https://codecov.io/gh/cimentadaj/perccalc/branch/master/graph/badge.svg)](https://codecov.io/gh/cimentadaj/perccalc?branch=master)

## Overview

`perccalc` is a direct implementation of the theoretical work of Reardon
(2011) where it is possible to estimate the difference between two
percentiles from an ordered categorical variable. More concretely, by
specifying an ordered categorical variable and a continuous variable,
this method can estimate differences in the continuous variable between
percentiles of the ordered categorical variable. This brings forth a
relevant strategy to contrast ordered categorical variables which
usually have alternative continuous measures to the percentiles of the
continuous measures. Moreover, this opens an avenue for calculating
percentile distributions and percentile differences for ordered
categorical variables which don’t necessarily have an alternative
continuous measure such as job occupation classifications. With this
package I introduce two functions that apply the procedure.

The package has two main functions:

  - `perc_diff`, for calculating percentile differences
  - `perc_dist`, for calculating scores for all percentiles

## Installation

You can install and load the package with these commands:

``` r
install.packages("perccalc") # for stable version
# or
devtools::install_github("cimentadaj/perccalc") # for development version
```

## Usage

Suppose we have a dataset with one continuous variable and one
categorical variable:

``` r
library(perccalc)
library(dplyr)
library(ggplot2)


df <-
  tibble(
    continuous = rnorm(100) + 1:100,
    categorical = rep(letters[1:5], each = 20) %>% factor(ordered = TRUE),
    wt = rnorm(100, mean = 5)
  )
```

Note that the categorical variable has to be an ordered factor (this is
a requirement of both functions). For example, `perc_diff` calculates
percentile differences using both variables.

``` r
perc_diff(df, categorical, continuous, percentiles = c(90, 10))
#> difference         se 
#> 79.9991129  0.1945426
```

You can optionally add weights with the `weights` argument.

``` r
perc_diff(df, categorical, continuous, weights = wt, percentiles = c(90, 10))
#> difference         se 
#> 80.1534474  0.3172118
```

On the other hand, the `perc_dist` (short for percentile distribution)
allows you to estimate the score for every percentile.

``` r
perc_dist(df, categorical, continuous) %>%
  head()
#> # A tibble: 6 x 3
#>   percentile estimate std.error
#>        <int>    <dbl>     <dbl>
#> 1          1    0.961    0.0291
#> 2          2    1.92     0.0569
#> 3          3    2.89     0.0834
#> 4          4    3.85     0.109 
#> 5          5    4.82     0.133 
#> # … with 1 more row
```

This function also allows the use of weights.

## Documentation and Support

Please visit <https://cimentadaj.github.io/perccalc/> for documentation
and vignettes with real-world examples. In case you want to file an
issue or contribute in another way to the package, please follow this
[guide](https://github.com/cimentadaj/perccalc/blob/master/.github/CONTRIBUTING.md).
For questions about the functionality, feel free to file an issue on
Github.

  - Reardon, Sean F. “The widening academic achievement gap between the
    rich and the poor: New evidence and possible explanations.” Whither
    opportunity (2011): 91-116.
