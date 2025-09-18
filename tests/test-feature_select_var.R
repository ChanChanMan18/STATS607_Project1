library(testthat)
source('src/feature_select_var.R')
source('src/generate_sample.R')

# Generating samples for testing
sample1 <- generate_sample(200, 100, 5, 5, "gaussian", df = 4, eta = 1, seed = 82803)$X
sample2 <- generate_sample(1000, 100, 5, 5, "gaussian", df = 4, eta = 1, seed = 82803)$X
sample3 <- generate_sample(1000, 100, 10, 10, "gaussian", df = 4, eta = 1, seed = 82803)$X
sample4 <- generate_sample(1000, 100, 10, 10, "t", df = 4, eta = 1, seed = 82803)$X
sample5 <- generate_sample(5, 10, 5, 10, "t", df = 4, eta = 1, seed = 18)$X


# Checking that function selects correct number of columns
test_that("Incorrect number of columns selected",
          { expect_equal(ncol(feature_select_var(sample1, 10)$X_K_highest_var), 10)
            expect_equal(ncol(feature_select_var(sample2, 10)$X_K_highest_var), 10)
            expect_equal(ncol(feature_select_var(sample3, 10)$X_K_highest_var), 10)
            expect_equal(ncol(feature_select_var(sample4, 10)$X_K_highest_var), 10)
            expect_equal(ncol(feature_select_var(sample5, 10)$X_K_highest_var), 10)
            expect_equal(ncol(feature_select_var(sample5, 20)$X_K_highest_var), 20)
          })

# Checking that first column has highest empirical variance
test_that("First selected column of design should have highest empirical variance",
          { expect_gte(var(feature_select_var(sample1, 10)$X_K_highest_var[, 1]), var(sample1[, sample(ncol(sample1))])[1])
            expect_gte(var(feature_select_var(sample2, 10)$X_K_highest_var[, 1]), var(sample2[, sample(ncol(sample2))])[1])
            expect_gte(var(feature_select_var(sample3, 10)$X_K_highest_var[, 1]), var(sample3[, sample(ncol(sample3))])[1])
            expect_gte(var(feature_select_var(sample4, 10)$X_K_highest_var[, 1]), var(sample4[, sample(ncol(sample4))])[1])
            expect_gte(var(feature_select_var(sample5, 10)$X_K_highest_var[, 1]), var(sample5[, sample(ncol(sample5))])[1])
          })
