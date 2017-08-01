#' Calculate percentile differences from an ordered categorical variable and a continuous variable.
#'
#' @param data_model A data frame with at least the categorical and continuous variables from
#' to estimate tue percentile differences
#' @param categorical_var The bare unquoted name of the categorical variable. This variable SHOULD be
#' an ordered factor. If not, the function will stop.
#' @param continuous_var The bare unquoted name of the continuous variable from which to estimate
#' the percentiles
#' @param weights The bare unquoted name of the optional weight variable. If not specified, then estimation
#' is done without weights
#' @param percentiles A numeric vector of two numbers specifying which percentiles to subtract
#'
#' @return A vector with the percentile difference and it's associated standard error
#' @importFrom magrittr "%>%"
#' @export
#'
#' @examples
#'
#' library(dplyr)
#'
#' set.seed(23131)
#' N <- 1000
#' K <- 20
#'
#' toy_data <- tibble::tibble(id = 1:N,
#' score = rnorm(N, sd = 2),
#' type = rep(paste0("inc", 1:20), each = N/K),
#' wt = 1)
#'
#' # perc_diff(toy_data, type, score)
#' # type is not an ordered factor!
#'
#' toy_data <-
#' toy_data %>%
#' mutate(type = factor(type, levels = unique(type), ordered = TRUE))
#'
#'
#' perc_diff(toy_data, type, score, percentiles = c(90, 10))
#' perc_diff(toy_data, type, score, percentiles = c(50, 10))
#'
#' perc_diff(toy_data, type, score, weights = wt, percentiles = c(30, 10))
#'
perc_diff <- function(data_model,
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
    dplyr::mutate(cathi = cumsum(per),
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
    stats::lm(mean ~ x1 + x2 + x3, weights = 1/se_mean^2, data = .)

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


category_summary <- function(data_model,
                             categorical_var,
                             continuous_var,
                             weights,
                             prefix = "cat_") {

  stopifnot(all(c("ordered", "factor") %in% class(data_model[[categorical_var]])))
  data_model <- data_model[stats::complete.cases(data_model), ]

  category_columns <- levels(data_model[[categorical_var]])
  new_category_columns <- paste0(prefix, category_columns)

  data_model[new_category_columns] <-
    purrr::map(category_columns, ~ ifelse(data_model[[categorical_var]] == .x, 1, 0))

  perc <-
    data_model[, new_category_columns, drop = F] %>%
    colMeans(na.rm = TRUE)

  coefs <- purrr::map(new_category_columns, ~ {

    wts <-
      data_model %>%
      dplyr::filter_(paste0("`", .x, "`", " == 1")) %>%
      .[[weights]]

    data_model %>%
      dplyr::filter_(paste0("`", .x, "`", " == 1")) %>%
      stats::lm(stats::as.formula(paste(continuous_var, "~ 1")), weights = wts, data = .) %>%
      broom::tidy() %>%
      .[1, c(2, 3)]
  })

  coefs %>%
    stats::setNames(new_category_columns) %>%
    tibble::enframe() %>%
    tidyr::unnest() %>%
    tibble::add_column(perc) %>%
    `names<-`(c("income", "mean", "se_mean", "per"))
}
