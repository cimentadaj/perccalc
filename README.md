perccalc
========

[![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/perccalc)](http://cran.r-project.org/package=perccalc)

Overview
--------

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
    #>  difference          se 
    #> 79.80784221  0.08769345

You can optionally add weights with the `weights` argument.

    perc_diff(df, categorical, continuous, weights = wt, percentiles = c(90, 10))
    #> difference         se 
    #> 80.4464316  0.2480369

On the other hand, the `perc_dist` (short for percentile distribution)
allows you to estimate the score for every percentile.

    perc_dist(df, categorical, continuous) %>%
      head()
    #>   percentile  estimate  std.error
    #> 1          1 0.9843332 0.01299658
    #> 2          2 1.9698260 0.02539664
    #> 3          3 2.9564483 0.03721271
    #> 4          4 3.9441699 0.04845732
    #> 5          5 4.9329608 0.05914303
    #> 6          6 5.9227909 0.06928238

This function also allows the use of weights. For an example with a a
real-world dataset, please see the vignette example.

All the intelectual ideas come from Sean Reardon. The R implemention is
my own work.

-   Reardon, Sean F. "The widening academic achievement gap between the
    rich and the poor: New evidence and possible explanations." Whither
    opportunity (2011): 91-116.
