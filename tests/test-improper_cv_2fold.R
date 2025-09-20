library(testthat)

#
# Use: highlight code and CMD + Return
#

# Loading necessary functions
source("src/generate_sample.R")
source("src/improper_cv_2fold.R")

# Check function returns single MSE
test_that("returns a single nonnegative numeric MSE", {
  sample <- generate_sample(n = 400, p = 20, M = 5, C = 4,
                         distribution = "gaussian", df = 4, 
                         eta = 1, seed = 82803)
  mse <- improper_cv_2fold(data_list = sample, p = ncol(sample$X), K = 5,
                           sample_size = nrow(sample$X), seed = 82803)
  expect_true(is.numeric(mse))
  expect_length(mse, 1)
  expect_true(is.finite(mse))
  expect_gte(mse, 0)
})

# Check function works for both distributions
test_that("works for both gaussian and t distributions", {
  sample_gauss <- generate_sample(n = 250, p = 12, M = 3, C = 6,
                           distribution = "gaussian", 
                           df = 4, eta = 1, seed = 33)
  sample_t <- generate_sample(n = 250, p = 12, M = 3, C = 6,
                           distribution = "t", 
                           df = 6, eta = 1, seed = 44)
  
  mse_gauss <- improper_cv_2fold(sample_gauss, p = ncol(sample_gauss$X), K = 3,
                             sample_size = nrow(sample_gauss$X), seed = 1)
  mse_t <- improper_cv_2fold(sample_t, p = ncol(sample_t$X), K = 3,
                             sample_size = nrow(sample_t$X), seed = 1)
  
  expect_true(is.finite(mse_gauss))
  expect_true(is.finite(mse_t))
})

test_that("input validation: y/X length mismatch errors", {
  sample <- generate_sample(n = 100, p = 8, M = 3, C = 4,
                         distribution = "gaussian", df = 4, eta = 1, seed = 66)
  bad <- list(X = sample$X, y = sample$y[-1])  
  expect_error(
    improper_cv_2fold(bad, p = ncol(sample$X), K = 3, sample_size = nrow(sample$X), seed = 1),
    "Length of y must match nrow\\(covariates\\)"
  )
})

test_that("Test K edge cases", {
  dat <- generate_sample(n = 200, p = 9, M = 2, C = 5,
                         distribution = "gaussian", df = 4, eta = 1, seed = 55)
  
  mse1 <- improper_cv_2fold(dat, p = ncol(dat$X), K = 1,
                          sample_size = nrow(dat$X), seed = 9)
  expect_true(is.finite(mse1))
  
  mse_all <- improper_cv_2fold(dat, p = ncol(dat$X), K = ncol(dat$X),
                             sample_size = nrow(dat$X), seed = 9)
  expect_true(is.finite(mse_all))
})

test_that("reproducible for identical seeds, different for different seeds", {
  dat <- generate_sample(n = 250, p = 12, M = 3, C = 6,
                         distribution = "gaussian", df = 4, eta = 1, seed = 22)
  a1 <- improper_cv_2fold(dat, p = ncol(dat$X), K = 3,
                        sample_size = nrow(dat$X), seed = 82803)
  a2 <- improper_cv_2fold(dat, p = ncol(dat$X), K = 3,
                        sample_size = nrow(dat$X), seed = 82803)
  expect_identical(a1, a2)
  
  b1 <- improper_cv_2fold(dat, p = ncol(dat$X), K = 3,
                        sample_size = nrow(dat$X), seed = 112195)
  # Different seeds → different fold split → very likely different MSE
  expect_false(identical(a1, b1))
})
