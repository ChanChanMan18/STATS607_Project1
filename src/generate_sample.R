
generate_sample <- function(n = 200,
                            p = 100,
                            M = 5,
                            C = 5,
                            distribution = c("gaussian", "t"),
                            df = 4,
                            eta = 1,
                            seed) {
  
  distribution <- match.arg(distribution)
  
  ### Generating design X from chosen distribution
  if (distribution == "gaussian") {
    
    X <- matrix(rnorm(n * p), nrow = n, ncol = p)
    
  } else {
    
    ### t-distribution with 'df' degrees of freedom if not Gaussian
    X <- matrix(rt(n * p, df = df), nrow = n, ncol = p)
    
  }
  
  ### Scale the appropriate columns by C
  if (M > 0) {
    X[, 1:M] <- C * X[, 1:M]
  }
  
  ### Generating betas from normal distribution
  beta_true <- rnorm(p, mean = 0, sd = 1)
  
  ### Generating the error terms 
  sigma2 <- eta * ((p - M) + (C^2) * M) # Noise variance
  eps <- rnorm(n, mean = 0, sd = sqrt(sigma2)) # The error
  
  ### Generating the output y
  y <- as.numeric(X %*% beta_true + eps)
  
  ### Compressing output
  return_list <- list(X = X, y = y, 
                      betaTrue = beta_true, 
                      sigma2 = sigma2)
  
  return(return_list)
  
}
