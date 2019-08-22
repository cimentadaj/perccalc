#' Calculate a distribution of percentiles from an ordered categorical variable
#'  and a continuous variable.
#'
#' @param data_model A data frame with at least the categorical and continuous
#'  variables from which to estimate the percentiles
#' @param categorical_var The bare unquoted name of the categorical variable.
#'  This variable \strong{should} be an ordered factor. If not, the function
#'  will raise an error.
#' @param continuous_var The bare unquoted name of the continuous variable from
#'  which to estimate the percentiles
#' 
#' @param weights The bare unquoted name of the optional weight variable.
#'  If not specified, then equal weights are assumed.
#'
#' @details \code{perc_dist} drops missing observations silently for calculating
#' the linear combination of coefficients.
#'
#' @return A data frame with the scores and standard errors for each percentile
#' 
#' @export
#'
#' @examples
#'
#' set.seed(23131)
#' N <- 1000
#' K <- 20
#'
#' toy_data <- data.frame(id = 1:N,
#'                        score = rnorm(N, sd = 2),
#'                        type = rep(paste0("inc", 1:20), each = N/K),
#'                        wt = 1)
#'
#'
#' # perc_diff(toy_data, type, score)
#' # type is not an ordered factor!
#'
#' toy_data$type <- factor(toy_data$type, levels = unique(toy_data$type), ordered = TRUE)
#'
#' perc_dist(toy_data, type, score)
perc_dist <- function(data_model,
                      categorical_var,
                      continuous_var,
                      weights = NULL) {
  categorical_var <- as.character(substitute(categorical_var))
  continuous_var <- as.character(substitute(continuous_var))

  weights <- as.character(substitute(weights))
  weights <- if (length(weights) == 0) NULL else weights

  data_model <-
    category_summary(
      data_model,
      categorical_var,
      continuous_var,
      weights
    )

  model <- linear_calculation(data_model)
  
  all_lcmb <- lapply(1:100, function(.x) {

    d1 <- (.x) / 100
    d2 <- (.x ^ 2) / (100 ^ 2)
    d3 <- (.x ^ 3) / (100 ^ 3)

    linear_combination <-
      multcomp::glht(
        model = model,
        linfct = paste0(d1, "*x1 + ", d2, "*x2 + ", d3, "*x3", " = 0")
      )

    linear_combination
  })


  all_perc <-
    lapply(all_lcmb, function(.x) {

      not_enough_categories <- is.nan(stats::vcov(.x))
      se <- if (not_enough_categories) NA else multcomp::adjusted()(.x)$sigma
      linear_diff <- stats::coef(.x)
      c("estimate" = unname(linear_diff), "std.error" = unname(se))
    })

  # If not enough categories, this should be true for all percentiles
  not_enough_categories <- is.nan(stats::vcov(all_lcmb[[100]]))

  if (not_enough_categories) {

    warning(
      "Too few categories in categorical variable to estimate the variance-covariance matrix and standard errors. Proceeding without estimated standard errors but perhaps you should increase the number of categories" #nolintr
    )
    # Only estimates (pos 1)
    estimates <- lapply(all_perc, `[`, 1)
    exclude_se <- TRUE
  } else {
    # Estimates + std.error (column 1 and 2)
    estimates <- lapply(all_perc, `[`, c(1, 2))
    exclude_se <- FALSE
  }

  percentile_data <-
    data.frame(
      percentile = 1:100,
      do.call(rbind, estimates)
    )

  if (exclude_se) percentile_data <- percentile_data[, c(1, 2)]

  tibble::as_tibble(percentile_data)
}
