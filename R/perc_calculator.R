#' Calculate percentiles from an ordered categorical variable and a continuous variable.
#'
#' @param data_model A data frame with at least the categorical and continuous variables from
#' to estimate the percentiles
#' @param categorical_var The bare unquoted name of the categorical variable. This variable SHOULD be
#' an ordered factor. If not, the function will stop.
#' @param continuous_var The bare unquoted name of the continuous variable from which to estimate
#' the percentiles
#' @param weights The bare unquoted name of the optional weight variable. If not specified, then estimation
#' is done without weights
#'
#' @return A data frame with the scores and standard errors for each percentiles
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
#' # perc_calculator(toy_data, type, score)
#' # type is not an ordered factor!
#'
#' toy_data <-
#' toy_data %>%
#' mutate(type = factor(type, levels = unique(type), ordered = TRUE))
#'
#'
#' perc_calculator(toy_data, type, score)
#'
perc_calculator <- function(data_model,
                            categorical_var,
                            continuous_var,
                            weights) {
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

  all_perc <- purrr::map(1:100, ~ {
    d1 <- (.x)/100
    d2 <- (.x^2)/(100^2)
    d3 <- (.x^3)/(100^3)

    lcmb <- multcomp::glht(model_data,
                     linfct = paste0(d1, '*x1 + ', d2, '*x2 + ', d3, '*x3', " = 0")) %>%
      summary() %>%
      broom::tidy()

    diff_hip_lop <- lcmb[1, 3, drop = TRUE]
    se_hip_lop <- lcmb[1, 4, drop = TRUE]

    c(score = diff_hip_lop, se = se_hip_lop)
  })

  all_perc %>%
    stats::setNames(1:100) %>%
    purrr::reduce(bind_rows) %>%
    transmute(percentile = 1:100, score, se)
}
