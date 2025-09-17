#' Perform the full LOO cross-validation analysis in Moscovich and Rosset (2022)
#' 
#' Input: 
#' p - number of predictors
#' M - 
#' input_sample - sample as generated according to function generate_sample.R,
#'                which in turn follows the sampling procedure described in the
#'                paper
#' 

source('src/generate_sample.R')
source('src/improper_cv_LOO.R')
source('src/proper_cv_LOO.R')
source('src/improper_cv_2fold.R')
source('src/proper_cv_2fold.R')

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
                                cross_val = "2fold",
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
    
    print(paste0("Beginning sample size: ", n))
    mse_vec_incorrect <- rep(0, num_runs)
    mse_vec_correct <- rep(0, num_runs)
    
    for (j in seq_len(num_runs)) {
      
      k <- k + 1L
      seed <- seed_stream[k]
      
      # Generate Dataset
      sample <- generate_sample(n = n, 
                                p = p, 
                                M = M,
                                C = C,
                                distribution = distribution,
                                df = df, 
                                eta = eta,
                                seed = seed)
      
      if (cross_val == '2fold') {
        
        # Computing MSE for Incorrect Pipeline, 2 fold
        mse_vec_incorrect[j] <- improper_cv_2fold(sample,
                                                  p,
                                                  K, 
                                                  n,
                                                  seed)
        
        # Computing MSE for Correct Pipeline, 2 fold
        mse_vec_correct[j] <- proper_cv_2fold(sample,
                                              p,
                                              K,
                                              n,
                                              seed)
      } else {
        
        # Computing MSE for Incorrect Pipeline, LOO
        mse_vec_incorrect[j] <- improper_cv_LOO(sample, 
                                            p,
                                            K,
                                            n) # numfolds
        
        # Computing MSE for Correct Pipeline, LOO
        mse_vec_correct[j] <- proper_cv_LOO(sample,
                                        p,
                                        K,
                                        n) # numfolds
      }
    }
    
    mse_incorrect[n_idx] <- mean(mse_vec_incorrect)
    mse_correct[n_idx] <- mean(mse_vec_correct)
  }
  
  list(mse_incorrect = mse_incorrect,
       mse_correct = mse_correct)
}