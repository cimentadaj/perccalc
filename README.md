---
output:
  github_document:
    html_preview: false
---

## Overview



Reardon (2011) introduced a very interesting concept in which he calculates percentile differences from ordered categorical variables. He explains his procedure very much in detail in the appendix of the book chapter but no formal implementation has been yet available on the web. With this package I introduce two functions that apply the procedure.

You can install and load the package with these commands:

```r
devtools::install_github("cimentadaj/perccalc")
library(perccalc)
```

The package has two main functions:

* `perc_diff`, for calculating percentile differences
* `perc_calculator`, for calculating scores for all percentiles

and each one performs percentile calculations. Suppose we have a dataset with one continuous variable and one categorical variable:



```r
library(tidyverse)

df <-
  tibble(
  continuous = rnorm(100) + 1:100,
  categorical = rep(letters[1:5], each = 20) %>% factor(ordered = TRUE))
```

Note that the categorical variable has to be an ordered factor (this is a requirement of both functions). For example, `perc_calc` calculates percentile differences using both variables.


```r
perc_diff(df, categorical, continuous, percentiles = c(90, 10))
#> difference         se 
#> 79.6735994  0.2235257
```

You can also use the `weights` argument to specify weights. On the other hand, the `perc_calculator` allows you to estimate the score for every percentile.


```r
perc_calculator(df, categorical, continuous)
#> # A tibble: 100 x 3
#>   percentile     score         se
#>        <int>     <dbl>      <dbl>
#> 1          1 0.9924821 0.03280493
#> 2          2 1.9847132 0.06411148
#> 3          3 2.9767040 0.09395110
#> 4          4 3.9684654 0.12235524
#> 5          5 4.9600081 0.14935537
#> # ... with 95 more rows
```

This function also allows the use of weights. For an example with a a real-world dataset, please see the vignette example.

All the intelectual ideas come from Sean Reardon. The R implemention is my own work.

- Reardon, Sean F. "The widening academic achievement gap between the rich and the poor: New evidence and possible explanations." Whither opportunity (2011): 91-116.
