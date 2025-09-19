library(testthat)

# Generating samples for testing
sample1 <- generate_sample(200, 100, 5, 5, "gaussian", df = 4, eta = 1, seed = 82803)$X
sample2 <- generate_sample(1000, 100, 5, 5, "gaussian", df = 4, eta = 1, seed = 82803)$X
sample3 <- generate_sample(1000, 100, 10, 10, "gaussian", df = 4, eta = 1, seed = 82803)$X
sample4 <- generate_sample(1000, 100, 10, 10, "t", df = 4, eta = 1, seed = 82803)$X
sample5 <- generate_sample(5, 10, 5, 10, "t", df = 4, eta = 1, seed = 18)$X

# Check that function selects correct number of columns
test_that("Incorrect number of columns selected",
          { expect_equal(ncol(feature_select_var(sample1, 10)$X_K_highest_var), 10)
            expect_equal(ncol(feature_select_var(sample2, 10)$X_K_highest_var), 10)
            expect_equal(ncol(feature_select_var(sample3, 10)$X_K_highest_var), 10)
            expect_equal(ncol(feature_select_var(sample4, 10)$X_K_highest_var), 10)
          })

# Check that first column has highest empirical variance
test_that("First selected column of design should have highest empirical variance",
          { expect_gte(var(feature_select_var(sample1, 10)$X_K_highest_var[, 1]), 
                       var(sample1[, sample(ncol(sample1))])[1])
            expect_gte(var(feature_select_var(sample2, 10)$X_K_highest_var[, 1]), 
                       var(sample2[, sample(ncol(sample2))])[1])
            expect_gte(var(feature_select_var(sample3, 10)$X_K_highest_var[, 1]), 
                       var(sample3[, sample(ncol(sample3))])[1])
            expect_gte(var(feature_select_var(sample4, 10)$X_K_highest_var[, 1]), 
                       var(sample4[, sample(ncol(sample4))])[1])
          })

# Check incorrect input for K
test_that("Check input for K", {
  
  expect_error(feature_select_var(sample1, K = "2"),
               "K must be a positive whole number", fixed = TRUE)
  expect_error(feature_select_var(sample2, K = 0),
               "K must be a positive whole number", fixed = TRUE)
  expect_error(feature_select_var(sample3, K = -3),
               "K must be a positive whole number", fixed = TRUE)
  expect_error(feature_select_var(sample4, K = 2.5),
               "K must be a positive whole number", fixed = TRUE)
  expect_error(feature_select_var(sample1, K = c(1, 2)),
               "K must be a positive whole number", fixed = TRUE)
  
  expect_error(feature_select_var(sample2, K = ncol(sample2) + 1),
               "K cannot exceed number of columns of design matrix", fixed = TRUE)
})

