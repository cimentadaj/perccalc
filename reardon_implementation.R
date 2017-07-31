# Create package with function
# Create document that shows that it works with one PISA
# and show the Stata results.

library(tidyverse)

category_summary <- function(data_model,
                             categorical_var,
                             continuous_var,
                             weights,
                             prefix = "cat_") {
  
  stopifnot(class(data_model[[categorical_var]]) %in% c("ordered", "factor"))
  data_model <- data_model[complete.cases(data_model), ]
  
  category_columns <- levels(data_model[[categorical_var]])
  new_category_columns <- paste0(prefix, category_columns)
  
  data_model[new_category_columns] <- 
    map(category_columns, ~ ifelse(data_model[[categorical_var]] == .x, 1, 0))
  
  perc <-
    data_model[, new_category_columns, drop = F] %>%
    colMeans(na.rm = TRUE)

  coefs <- map(new_category_columns, ~ {

   
    wts <-
      data_model %>%
      filter_(paste0("`", .x, "`", " == 1")) %>%
      .[[weights]]
    
    data_model %>%
      filter_(paste0("`", .x, "`", " == 1")) %>%
      lm(as.formula(paste(continuous_var, "~ 1")), weights = wts, data = .) %>%
      broom::tidy() %>%
      .[1, c(2, 3)]
  })

  coefs %>%
    setNames(new_category_columns) %>%
    enframe() %>%
    unnest() %>%
    add_column(perc) %>%
    `names<-`(c("income", "mean", "se_mean", "per"))
}

# Remember that the categorical variable needs to be an ordered factor!
# Remember that the weights can be not-specified!

perc_calculator <- function(data_model,
                            categorical_var,
                            continuous_var,
                            weights,
                            percentiles = c(90, 10)) {
  variable_name <- as.character(substitute(categorical_var))
  continuous_name <- as.character(substitute(continuous_var))
  weights <- as.character(substitute(weights))
  
  data_model <- category_summary(data_model, variable_name, continuous_name, weights)
  
  data_ready <-
    data_model %>%
    mutate(cathi = cumsum(per),
           catlo = dplyr::lag(cathi, default = 0),
           catmid = (catlo+cathi) / 2,
           x1 = catmid,
           x2 = catmid^2 + ((cathi-catlo)^2)/12,
           x3 = catmid^3 + ((cathi-catlo)^2)/4,
           cimnhi = mean + 1.96 * se_mean,
           cimnlo = mean - 1.96 * se_mean
    )
  
  model_data <-
    data_ready %>%
    lm(mean ~ x1 + x2 + x3, weights = 1/se_mean^2, data = .)
  
  hi_p <- percentiles[1]
  lo_p <- percentiles[2]
  
  d1 <- (hi_p - lo_p)/100
  d2 <- (hi_p^2 - lo_p^2)/(100^2)
  d3 <- (hi_p^3 - lo_p^3)/(100^3)
  
  lcmb <-
    multcomp::glht(model_data,
                   linfct = paste0(d1, '*x1 + ', d2, '*x2 + ', d3, '*x3', " = 0")) %>%
    summary() %>%
    broom::tidy()
  
  diff_hip_lop <- lcmb[1, 3, drop = TRUE]
  se_hip_lop <- lcmb[1, 4, drop = TRUE]
  
  c(difference = diff_hip_lop, se = se_hip_lop)
}

library(PISA2012lite)
library(matrixStats)

ger_student <- student2012 %>%
  filter(CNT == "Germany") %>%
  select(CNT, STIDSTD, matches("^PV*.MATH$")) %>%
  transmute(CNT, STIDSTD,
            avg_score = rowMeans(student2012[student2012$CNT == "Germany", paste0("PV", 1:5, "MATH")]))

ger_parent <- parent2012 %>% filter(CNT == "Germany") %>% select(CNT, STIDSTD, PA07Q01)
ger_weights <- student2012weights %>% filter(CNT == "Germany") %>% select(CNT, STIDSTD, W_FSTUWT)

dataset_ready <-
  ger_student %>%
  left_join(ger_parent, by = c("CNT", "STIDSTD")) %>%
  left_join(ger_weights, by = c("CNT", "STIDSTD")) %>%
  as_tibble() %>%
  mutate(PA07Q01 = factor(PA07Q01, ordered = TRUE))

# for (i in unique(data_model$type)) {
#   data_model[i] <- ifelse(data_model$type == i, 1, 0)
# }

# Change dataset names to run the stata code
# What does Stata do with the missing values? Excludes only rows which are complete? or deletes
# a row even if it only has an NA in the category variable

perc_calculator(dataset_ready, PA07Q01, avg_score, percentiles = c(70, 30))
