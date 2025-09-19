library(testthat)

# Checking dimensions of outputs
test_that("Dimensions of outputs incorrect",
          {
            # Generating samples for testing
            sample1 <- generate_sample(200, 100, 5, 5, "gaussian", df = 4, eta = 1, seed = 82803)
            sample2 <- generate_sample(1000, 100, 5, 5, "gaussian", df = 4, eta = 1, seed = 82803)
            sample3 <- generate_sample(40, 30, 10, 10, "gaussian", df = 4, eta = 1, seed = 82803)
            sample4 <- generate_sample(1000, 100, 10, 10, "t", df = 4, eta = 1, seed = 82803)
            sample5 <- generate_sample(5, 10, 5, 10, "t", df = 4, eta = 1, seed = 18)
            
            expect_equal(nrow(sample1$X), length(sample1$y))
            expect_equal(nrow(sample2$X), length(sample2$y))
            expect_equal(nrow(sample3$X), length(sample3$y))
            expect_equal(nrow(sample4$X), length(sample4$y))
            expect_equal(nrow(sample5$X), length(sample5$y))
            
            expect_equal(ncol(sample1$X), length(sample1$beta_true))
            expect_equal(ncol(sample2$X), length(sample2$beta_true))
            expect_equal(ncol(sample3$X), length(sample3$beta_true))
            expect_equal(ncol(sample4$X), length(sample4$beta_true))
            expect_equal(ncol(sample5$X), length(sample5$beta_true))
            
          })

# Check that errors thrown by incorrect input
test_that("Numeric and positivity check", {
  
  expect_error(generate_sample(n = "200", p = 10, M = 2, C = 2, df = 4, eta = 1, seed = 1),
               "must be numeric", fixed = TRUE)
  expect_error(generate_sample(n = 200, p = 10, M = 2, C = 2, df = "4", eta = 1, seed = 1),
               "must be numeric", fixed = TRUE)
  
  # Non-positive values
  expect_error(generate_sample(n = 0, p = 10, M = 2, C = 2, df = 4, eta = 1, seed = 1),
               "must be positive", fixed = TRUE)
  expect_error(generate_sample(n = 200, p = -1, M = 2, C = 2, df = 4, eta = 1, seed = 1),
               "must be positive", fixed = TRUE)
  expect_error(generate_sample(n = 200, p = 10, M = 2, C = 2, df = 4, eta = 0, seed = 1),
               "must be positive", fixed = TRUE)
})

# Check non-integer inputs
test_that("Check integers for n, p, M, df", {
  expect_error(generate_sample(n = 10.5, p = 10, M = 2, C = 2, df = 4, eta = 1, seed = 1),
               "must be whole numbers", fixed = TRUE)
  expect_error(generate_sample(n = 200, p = 10.2, M = 2, C = 2, df = 4, eta = 1, seed = 1),
               "must be whole numbers", fixed = TRUE)
  expect_error(generate_sample(n = 200, p = 10, M = 2.7, C = 2, df = 4, eta = 1, seed = 1),
               "must be whole numbers", fixed = TRUE)
  expect_error(generate_sample(n = 200, p = 10, M = 2, C = 2, df = 4.3, eta = 1, seed = 1),
               "must be whole numbers", fixed = TRUE)
})
