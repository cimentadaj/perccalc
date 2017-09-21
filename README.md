Overview
--------

[![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/perccalc)](http://cran.r-project.org/package=perccalc)

Reardon (2011) introduced a very interesting concept in which he
calculates percentile differences from ordered categorical variables. He
explains his procedure very much in detail in the appendix of the book
chapter but no formal implementation has been yet available on the web.
With this package I introduce two functions that apply the procedure.

The package has two main functions:

-   `perc_diff`, for calculating percentile differences
-   `perc_dist`, for calculating scores for all percentiles

Installation
------------

You can install and load the package with these commands:

    devtools::install_github("cimentadaj/perccalc")
    library(perccalc)

Usage
-----

Suppose we have a dataset with one continuous variable and one
categorical variable:

    library(tidyverse)

    df <-
      tibble(
        continuous = rnorm(100) + 1:100,
        categorical = rep(letters[1:5], each = 20) %>% factor(ordered = TRUE),
        wt = rnorm(100, mean = 5)
      )

Note that the categorical variable has to be an ordered factor (this is
a requirement of both functions). For example, `perc_diff` calculates
percentile differences using both variables.

    perc_diff(df, categorical, continuous, percentiles = c(90, 10))
    #> difference         se 
    #> 80.2036783  0.1780654

You can optionally add weights with the `weights` argument.

    perc_diff(df, categorical, continuous, weights = wt, percentiles = c(90, 10))
    #> difference         se 
    #> 81.5266344  0.1261735

On the other hand, the `perc_dist` (short for percentile distribution)
allows you to estimate the score for every percentile.

    perc_dist(df, categorical, continuous) %>%
      head()
    #>   percentile estimate  std.error
    #> 1          1 1.053517 0.02597837
    #> 2          2 2.104579 0.05076806
    #> 3          3 3.153235 0.07439407
    #> 4          4 4.199533 0.09688140
    #> 5          5 5.243521 0.11825505
    #> 6          6 6.285246 0.13854007

This function also allows the use of weights. For an example with a a
real-world dataset, please see the vignette example.

All the intelectual ideas come from Sean Reardon. The R implemention is
my own work.

-   Reardon, Sean F. "The widening academic achievement gap between the
    rich and the poor: New evidence and possible explanations." Whither
    opportunity (2011): 91-116.
