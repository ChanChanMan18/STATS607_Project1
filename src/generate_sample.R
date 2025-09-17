#' Generate data according to main example (Section 4) of 
#' Moscovich and Rosset (2022)
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
                            distribution = c("gaussian", "t"),
                            df = 4,
                            eta = 1,
                            seed = seed) {
  
  set.seed(seed)
  distribution <- match.arg(distribution)
  
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
