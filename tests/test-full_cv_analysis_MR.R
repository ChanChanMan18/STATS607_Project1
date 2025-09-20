library(testthat)

#
# Use: highlight script and CMD + Return
#

# Loading necessary functions
source('src/generate_sample.R')
source('src/improper_cv_LOO.R')
source('src/proper_cv_LOO.R')
source('src/improper_cv_2fold.R')
source('src/proper_cv_2fold.R')
source('src/full_cv_analysis_MR.R')

test_that("Provides warning and outputs when master_seed is NULL", {
  expect_warning(
    out <- full_cv_analysis_MR(
      p = 20, K = 3, M = 3, C = 4,
      distribution = "gaussian", df = 4, eta = 1,
      num_runs = 2,
      sample_size = c(30, 40),
      cross_val = "2fold",
      master_seed = NULL
    ),
    "Seed should be set for reproducibility",
    fixed = FALSE
  )
  expect_equal(length(out$mse_incorrect), 2)
  expect_equal(length(out$mse_correct), 2)
})

test_that("MSE vectors have correct length and are nonnegative", {
  out <- full_cv_analysis_MR(
    p = 30, K = 5, M = 5, C = 5,
    distribution = "gaussian", df = 4, eta = 1,
    num_runs = 3,
    sample_size = c(60, 80),
    cross_val = "2fold",
    master_seed = 82803
  )
  expect_type(out, "list")
  expect_named(out, c("mse_incorrect", "mse_correct"))
  
  expect_true(is.numeric(out$mse_incorrect))
  expect_true(is.numeric(out$mse_correct))
  expect_equal(length(out$mse_incorrect), 2)
  expect_equal(length(out$mse_correct), 2)
  
  expect_true(all(out$mse_incorrect >= 0))
  expect_true(all(out$mse_correct >= 0))
})


test_that("works with LOO ", {
  out <- full_cv_analysis_MR(
    p = 25, K = 4, M = 5, C = 3,
    distribution = "t", df = 6, eta = 1,
    num_runs = 2,
    sample_size = c(20, 35, 47),
    cross_val = "LOO",
    master_seed = 1995
  )
  expect_equal(length(out$mse_incorrect), 3)
  expect_equal(length(out$mse_correct), 3)
  expect_true(all(is.finite(out$mse_incorrect)))
  expect_true(all(is.finite(out$mse_correct)))
})

test_that("num_runs must be a positive whole number", {
  expect_error(
    full_cv_analysis_MR(num_runs = 0, sample_size = c(50, 60), master_seed = 82803),
    "num_runs must be a positive whole number",
    fixed = TRUE
  )
  expect_error(
    full_cv_analysis_MR(num_runs = 2.5, sample_size = c(50, 60), master_seed = 82803),
    "num_runs must be a positive whole number",
    fixed = TRUE
  )
  expect_error(
    full_cv_analysis_MR(num_runs = c(1, 2), sample_size = c(50, 60), master_seed = 82803),
    "num_runs must be a positive whole number",
    fixed = TRUE
  )
  expect_error(
    full_cv_analysis_MR(num_runs = "3", sample_size = c(50, 60), master_seed = 82803),
    "num_runs must be a positive whole number",
    fixed = TRUE
  )
})








