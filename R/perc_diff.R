#' Calculate percentile differences from an ordered categorical variable
#' and a continuous variable.
#'
#' @param data_model A data frame with at least the categorical and continuous
#'  variables from which to estimate the percentile differences
#' @param categorical_var The bare unquoted name of the categorical variable.
#'  This variable SHOULD be an ordered factor. If not, will raise an error.
#' @param continuous_var The bare unquoted name of the continuous variable from
#'  which to estimate the percentiles
#' @param weights The bare unquoted name of the optional weight variable.
#'  If not specified, then estimation is done without weights
#' @param percentiles A numeric vector of two numbers specifying which
#'  percentiles to subtract
#'
#' @details \code{perc_diff} drops missing observations silently for calculating
#' the linear combination of coefficients.
#' 
#' @return \code{perc_diff} returns a vector with the percentile difference and
#' its associated standard error. \code{perc_diff_df} returns the same but as
#' a data frame.
#' @export
#'
#' @examples
#'
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
#' perc_diff(toy_data, type, score, percentiles = c(90, 10))
#' perc_diff(toy_data, type, score, percentiles = c(50, 10))
#'
#' perc_diff(toy_data, type, score, weights = wt, percentiles = c(30, 10))
#' # Results as data frame
#' perc_diff_df(toy_data, type, score, weights = wt, percentiles = c(30, 10))
#'
perc_diff <- function(data_model,
                      categorical_var,
                      continuous_var,
                      weights = NULL,
                      percentiles = c(90, 10)) {

  categorical_var <- as.character(substitute(categorical_var))
  continuous_var <- as.character(substitute(continuous_var))
  weights <- as.character(substitute(weights))

  perc_diff_(data_model = data_model,
             categorical_var = categorical_var,
             continuous_var = continuous_var,
             weights = weights,
             percentiles = percentiles)

}

#' @rdname perc_diff
#' @export
perc_diff_df <- function(data_model,
                         categorical_var,
                         continuous_var,
                         weights = NULL,
                         percentiles = c(90, 10)) {

  categorical_var <- as.character(substitute(categorical_var))
  continuous_var <- as.character(substitute(continuous_var))
  weights <- as.character(substitute(weights))
  
  res <- perc_diff_(data_model = data_model,
                   categorical_var = categorical_var,
                   continuous_var = continuous_var,
                   weights = weights,
                   percentiles = percentiles)
  
  as.data.frame(lapply(res, identity))
}

perc_diff_ <- function(data_model,
                      categorical_var,
                      continuous_var,
                      weights = NULL,
                      percentiles = c(90, 10)) {

  weights <- if (length(weights) == 0) NULL else weights

  data_model <-
    category_summary(
      data_model,
      categorical_var,
      continuous_var,
      weights
    )

  model <- linear_calculation(data_model)
  hi_p <- percentiles[1]
  lo_p <- percentiles[2]

  d1 <- (hi_p - lo_p) / 100
  d2 <- (hi_p ^ 2 - lo_p ^ 2) / (100 ^ 2)
  d3 <- (hi_p ^ 3 - lo_p ^ 3) / (100 ^ 3)

  linear_combination <-
    multcomp::glht(
      model = model,
      linfct = paste0(d1, "*x1 + ", d2, "*x2 + ", d3, "*x3", " = 0")
    )

  not_enough_categories <- is.nan(stats::vcov(linear_combination))

  if (not_enough_categories) {

    warning(
      "Too few categories in categorical variable to estimate the variance-covariance matrix and standard errors. Proceeding without estimated standard errors but perhaps you should increase the number of categories" #nolintr
    )

    se_hip_lop <- NA

  } else {

    se_hip_lop <- multcomp::adjusted()(linear_combination)$sigma

  }

  diff_hip_lop <- stats::coef(linear_combination)

  c(difference = unname(diff_hip_lop), se = unname(se_hip_lop))
}

category_summary <- function(data_model,
                             categorical_var,
                             continuous_var,
                             weights,
                             prefix = "cat_") {

  is_ordered_fct <- all(
    c("ordered", "factor") %in% class(data_model[[categorical_var]])
  )

  if (!is_ordered_fct) stop("`categorical_var` should be an ordered factor")

  data_model$use_weights <-
    if (is.null(weights)) rep(1, nrow(data_model)) else data_model[[weights]]

  data_model <- data_model[, c(categorical_var, continuous_var, "use_weights")]

  data_model <- data_model[stats::complete.cases(data_model), ]

  category_columns <- levels(data_model[[categorical_var]])
  new_cat_cols <- paste0(prefix, category_columns)

  data_model[new_cat_cols] <- lapply(category_columns, function(.x) {
    ifelse(data_model[[categorical_var]] == .x, 1, 0)
  })

  perc <-
    vapply(
      data_model[, new_cat_cols, drop = FALSE],
      stats::weighted.mean,
      w = data_model$use_weights,
      FUN.VALUE = numeric(1)
    )
    
  all_results <- lapply(new_cat_cols, function(.x) {

    category_selection <-
      eval(parse(text = paste0("`", .x, "`", " == 1")), data_model)

    model <- stats::lm(
      formula = stats::as.formula(paste(continuous_var, "~ 1")),
      weights = data_model$use_weights[category_selection],
      data = data_model[category_selection, ]
    )

    results <- summary(model)$coefficients[, c("Estimate", "Std. Error")]

    results
  })


  final_df <-
    data.frame(
      names(perc),
      do.call(rbind, all_results),
      perc
    )
  names(final_df) <- c("income", "mean", "se_mean", "per")

  tibble::as_tibble(final_df)
}

my_lag <- function(x, n = 1L, default = 0) {
  xlen <- length(x)
  n <- pmin(n, xlen)
  out <- c(rep(default, n), x[seq_len(xlen - n)])
  attributes(out) <- attributes(x)
  out
}


linear_calculation <- function(data_model) {

  data_model$cathi <- cumsum(data_model$per)
  data_model$catlo <- my_lag(data_model$cathi, default = 0)

  intermediate_calculation <- (data_model$cathi - data_model$catlo) ^ 2

  data_model$catmid <- (data_model$catlo + data_model$cathi) / 2
  data_model$x1 <- data_model$catmid
  data_model$x2 <- data_model$catmid ^ 2 +  intermediate_calculation / 12
  data_model$x3 <- data_model$catmid ^ 3 +  intermediate_calculation / 4
  data_model$cimnhi <- data_model$mean + 1.96 * data_model$se_mean
  data_model$cimnlo <- data_model$mean - 1.96 * data_model$se_mean

  model_data <-
    stats::lm(
      mean ~ x1 + x2 + x3,
      weights = 1 / data_model$se_mean ^ 2, data = data_model
    )

  model_data
}
