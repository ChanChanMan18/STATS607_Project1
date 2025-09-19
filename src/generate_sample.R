#' Generate data according to main example (Section 4) of 
#' Moscovich and Rosset (2022)
#' 
#' Usage:
#' 
#' 
#' Input:
#' n - number of samples
#' p - number of predictors
#' M - first M predictors scaled by constant C > 0
#' C - positive constant
#' distribution - select from gaussian or t-distribution (the two distributions)
#'                considered in the paper
#' df - degrees of freedom if distribution = "t"
#' eta - noise level
#' 
#' Output: 
#' list containing design matrix X, responses y, true coefficients beta_true,
#' and noise variance sigma2

generate_sample <- function(n = 200,
                            p = 100,
                            M = 5,
                            C = 5,
                            distribution = "gaussian",
                            df = 4,
                            eta = 1,
                            seed = seed) {
  
  if (!is.numeric(n) || !is.numeric(p) || !is.numeric(M) ||
      !is.numeric(C) || !is.numeric(df) || !is.numeric(eta)) {
    stop("Inputs n, p, M, C, df, and eta must be numeric.")
  }
  
  if (!(n > 0) || !(p > 0) || !(M > 0) || 
      !(C > 0) || !(df > 0) || !(eta > 0)) {
    stop("Inputs n, p, M, C, df, and eta must be positive")
  }
  
  if (!(n %% 1 == 0) || !(p %% 1 == 0) || 
      !(M %% 1 == 0) || !(df %% 1 == 0)) {
    stop("Inputs n, p, M, and df must be whole numbers")
  }
  
  allowed_dist <- c("gaussian", "t")
  if (!is.character(distribution) || 
      length(distribution) != 1L ||
      !match(tolower(distribution), allowed_dist, nomatch = 0L)) {
    stop("Distribution must be 'gaussian' or 't'")
  }
  
  # Set seed for reproducibility
  set.seed(seed)
  
  # Set up design according to authors and user input dist
  distribution <- tolower(distribution)
  if (distribution == "gaussian") {
    X <- matrix(rnorm(n * p), nrow = n, ncol = p)
  } else {
    X <- matrix(rt(n * p, df = df), nrow = n, ncol = p)
  }
  
  # Scaling first M predictors by constant C for larger variance
  if (M > 0) {
    X[, 1:M] <- C * X[, 1:M]
  }
  
  # Generate true betas from standard normal
  beta_true <- rnorm(p, mean = 0, sd = 1)
  
  # Generating error terms 
  sigma2 <- eta * ((p - M) + (C^2) * M) # Noise variance
  eps <- rnorm(n, mean = 0, sd = sqrt(sigma2)) 
  
  # Generating output y
  y <- as.numeric(X %*% beta_true + eps)
  
  output <- list(X = X, y = y, 
                 beta_true = beta_true, 
                 sigma2 = sigma2)
  
  return(output)
  
}
