# Toy dataset
df <-
  data.frame(
    continuous = rnorm(100) + 1:100,
    categorical = factor(rep(letters[1:5], each = 20), ordered = TRUE),
    wt = rnorm(100, mean = 5),
    # This is a test in it self. Checking that everything is computed
    # correctly with additional columns
    random_col1 = 1,
    random_col2 = 2,
    random_col3 = 3
  )

test_that("perc_diff returns the correct output", {

  # Output is numeric
  expect_is(perc_diff(df, categorical, continuous), "numeric")

  # is length 2
  expect_length(perc_diff(df, categorical, continuous), 2)

  # and returns a named vector
  expect_named(perc_diff(df, categorical, continuous))
})


test_that("perc_dist returns the correct output", {

  # Expect it's a data frame
  expect_is(perc_dist(df, categorical, continuous), "data.frame")

  # Has 100 rows based on percentiles
  expect_equal(nrow(perc_dist(df, categorical, continuous)), 100)

  # Has the number of columns
  expect_equal(ncol(perc_dist(df, categorical, continuous)), 3)

  # Expects all columns are numerics
  expect_identical(
    unique(sapply(perc_dist(df, categorical, continuous), mode)),
            "numeric")
})

smoking_data <- subset(MASS::survey, select = c("Sex", "Smoke", "Pulse"))

# If factor is not ordered, expect error
expect_error(perc_diff(smoking_data, Smoke, Pulse))

smoking_data$Smoke <-
  factor(
    smoking_data$Smoke,
    levels = c("Never", "Regul", "Occas", "Heavy"),
    ordered = TRUE
  )

test_that("When too few categories in df, correct output", {

  # Throws a warnings
  warn_result <- expect_warning(perc_diff(smoking_data, Smoke, Pulse),
                                regexp = "Too few categories in categorical variable to estimate the variance-covariance matrix and standard errors. Proceeding without estimated standard errors but perhaps you should increase the number of categories", #nolintr
                                fixed = TRUE)

  # Output is numeric
  expect_is(warn_result, "numeric")

  # is length 1
  expect_length(warn_result, 2)

  # and returns a named vector
  expect_named(warn_result)
})

test_that("perc_diff calculates value according to Reardon", {
  library(carData)

  set.seed(213141)
  data("GSSvocab")

  gss <- GSSvocab
  gss$ageGroup <- factor(gss$ageGroup, ordered = TRUE)
  gss$weight <- sample(1:3,
                       size = nrow(gss),
                       replace = TRUE,
                       prob = c(0.1, 0.5, 0.4))

  gss <- subset(gss,
                year == "1978",
                select = c("ageGroup", "vocab", "weight"))

  result <- unname(round(perc_diff(gss, ageGroup, vocab, weight), 4))

  expect_true(all.equal(c(0.1761, 0.3775), result))
})
