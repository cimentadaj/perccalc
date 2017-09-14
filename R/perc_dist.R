#' Calculate a distribution of percentiles from an ordered categorical variable
#'  and a continuous variable.
#'
#' @param data_model A data frame with at least the categorical and continuous
#'  variables from which to estimate the percentiles
#' @param categorical_var The bare unquoted name of the categorical variable.
#'  This variable SHOULD be an ordered factor. If not, the function will stop.
#' @param continuous_var The bare unquoted name of the continuous variable from
#'  which to estimate the percentiles
#' @param weights The bare unquoted name of the optional weight variable.
#'  If not specified, then estimation is done without weights
#'
#' @return A data frame with the scores and standard errors for each percentile
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
#' # perc_dist(toy_data, type, score)
#' # type is not an ordered factor!
#'
#' toy_data <-
#'  toy_data %>%
#'  mutate(type = factor(type, levels = unique(type), ordered = TRUE))
#'
#'
#' perc_dist(toy_data, type, score)
perc_dist <- function(data_model,
                            categorical_var,
                            continuous_var,
                            weights = NULL) {
  variable_name <- as.character(substitute(categorical_var))
  continuous_name <- as.character(substitute(continuous_var))

  weights <- as.character(substitute(weights))
  weights <- if (purrr::is_empty(weights)) NULL else weights

  data_model <-
    category_summary(
      data_model,
      variable_name,
      continuous_name,
      weights
    )

  data_model$cathi <- cumsum(data_model$per)
  data_model$catlo <- dplyr::lag(data_model$cathi, default = 0)

  intermediate_calculation <- ((data_model$cathi - data_model$catlo) ^ 2)

  data_model$catmid <- (data_model$catlo + data_model$cathi) / 2
  data_model$x1 <- data_model$catmid
  data_model$x2 <- data_model$catmid ^ 2 +  intermediate_calculation / 12
  data_model$x3 <- data_model$catmid ^ 3 +  intermediate_calculation / 4
  data_model$cimnhi <- data_model$mean + 1.96 * data_model$se_mean
  data_model$cimnlo <- data_model$mean - 1.96 * data_model$se_mean

  data_ready <- data_model

  model_data <- stats::lm(mean ~ x1 + x2 + x3,
                          weights = 1 / data_ready$se_mean ^ 2,
                          data = data_ready)

  all_lcmb <- purrr::map(1:100, ~ {

    d1 <- (.x) / 100
    d2 <- (.x ^ 2) / (100 ^ 2)
    d3 <- (.x ^ 3) / (100 ^ 3)

    linear_combination <-
      multcomp::glht(
        model = model_data,
        linfct = paste0(d1, "*x1 + ", d2, "*x2 + ", d3, "*x3", " = 0")
      )

    linear_combination
  })


  all_perc <-
    purrr::map(all_lcmb, ~ {
      enough_categories <- is.nan(stats::vcov(.x))

      if (enough_categories) {

        lcmb <- broom::tidy(.x)
        lcmb

      } else {
        lcmb <-
          broom::tidy(
            summary(
              .x
            )
          )
        lcmb
      }
    })

  enough_categories <-
    is.nan(
      stats::vcov(
        all_lcmb[[100]]
      )
    )

  if (enough_categories) {
    warning(
      "Too few categories in categorical variable to estimate the
      variance-covariance matrix and standard errors. Proceeding without
      estimated standard errors but perhaps you should increase the number
      of categories"
    )
    only_estimates <- purrr::map(all_perc, ~ .x[3])
    final_column_selection <- 2:1
  } else {
    only_estimates <- purrr::map(all_perc, ~ .x[c(3, 4)])
    final_column_selection <- c(3, 1, 2)
  }

  percentile_data <-
    purrr::reduce(
    stats::setNames(only_estimates, 1:100),
    dplyr::bind_rows
    )

  percentile_data$percentile <- 1:100

  percentile_data[, final_column_selection]
}
