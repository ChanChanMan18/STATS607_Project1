#' Perform the full LOO cross-validation analysis in Moscovich and Rosset (2022)
#' 
#' Input: 
#' p - number of predictors
#' M - 
#' input_sample - sample as generated according to function generate_sample.R,
#'                which in turn follows the sampling procedure described in the
#'                paper
#' 

full_cv_analysis_MR <- function(p = 1000,
                                K = 100,
                                M = 5,
                                C = 5,
                                distribution = "gaussian",
                                df = 4,
                                eta = 1,
                                num_runs = 1000,
                                sample_size = seq(from = 200,
                                                  to = 600,
                                                  by = 50),
                                master_seed = 82803) {
  
  # Precompute one unique seed per generate_sample call
  total_runs <- length(sample_size) * num_runs
  set.seed(master_seed)
  seed_stream <- sample.int(.Machine$integer.max,
                           total_runs,
                           replace = FALSE)
  
  mse_incorrect <- numeric(length(sample_size))
  mse_correct <- numeric(length(sample_size))
  
  k <- 0L
  for (n_idx in seq_along(sample_size)) {
    
    n <- sample_size[n_idx]
    mse_vec_incorrect <- rep(0, num_runs)
    mse_vec_correct <- rep(0, num_runs)
    
    for (j in seq_len(num_runs)) {
      
      k <- k + 1L
      seed <- seed_stream[k]
      
      ### Generate Dataset
      sample <- generate_sample(n = n, 
                                p = p, 
                                M = M,
                                C = C,
                                distribution = distribution,
                                df = df, 
                                eta = eta,
                                seed = seed)
      
      ### Computing MSE for Incorrect Pipeline
      mse_vec_incorrect[j] <- improper_cv(sample, 
                                          p,
                                          K,
                                          n) # numfolds
      
      ### Computing MSE for Correct Pipeline
      mse_vec_correct[j] <- proper_cv(sample,
                                      p,
                                      K,
                                      n) # numfolds
      
      
    }
    
    mse_incorrect[n_idx] <- mean(mse_vec_incorrect)
    mse_correct[n_idx] <- mean(mse_vec_correct)
    
  }
  
  list(mse_incorrect = mse_incorrect,
       mse_correct = mse_correct)
}