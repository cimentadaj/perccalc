
perccalc
========

[![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/perccalc)](http://cran.r-project.org/package=perccalc) [![Travis-CI Build Status](https://travis-ci.org/cimentadaj/perccalc.svg?branch=master)](https://travis-ci.org/cimentadaj/perccalc)

Overview
--------

Reardon (2011) introduced a very interesting concept in which he calculates percentile differences from ordered categorical variables. He explains his procedure very much in detail in the appendix of the book chapter but no formal implementation has been yet available on the web. With this package I introduce two functions that apply the procedure.

The package has two main functions:

-   `perc_diff`, for calculating percentile differences
-   `perc_dist`, for calculating scores for all percentiles

Installation
------------

You can install and load the package with these commands:

``` r
devtools::install_github("cimentadaj/perccalc") # for development version
# or
install.packages("perccalc") # for stable version
library(perccalc)
```

Usage
-----

Suppose we have a dataset with one continuous variable and one categorical variable:

``` r
library(dplyr)
library(ggplot2)


df <-
  tibble(
    continuous = rnorm(100) + 1:100,
    categorical = rep(letters[1:5], each = 20) %>% factor(ordered = TRUE),
    wt = rnorm(100, mean = 5)
  )
```

Note that the categorical variable has to be an ordered factor (this is a requirement of both functions). For example, `perc_diff` calculates percentile differences using both variables.

``` r
perc_diff(df, categorical, continuous, percentiles = c(90, 10))
#> difference         se 
#>  79.648697   0.337841
```

You can optionally add weights with the `weights` argument.

``` r
perc_diff(df, categorical, continuous, weights = wt, percentiles = c(90, 10))
#>   difference           se 
#> 80.165952682  0.004048703
```

On the other hand, the `perc_dist` (short for percentile distribution) allows you to estimate the score for every percentile.

``` r
perc_dist(df, categorical, continuous) %>%
  head()
#>   percentile estimate  std.error
#> 1          1 1.039075 0.04856837
#> 2          2 2.076428 0.09492464
#> 3          3 3.112086 0.13911520
#> 4          4 4.146079 0.18118645
#> 5          5 5.178434 0.22118480
#> 6          6 6.209181 0.25915670
```

This function also allows the use of weights. For an example with a a real-world dataset, please see the vignette example.

All the intelectual ideas come from Sean Reardon. The R implemention is my own work.

-   Reardon, Sean F. "The widening academic achievement gap between the rich and the poor: New evidence and possible explanations." Whither opportunity (2011): 91-116.
