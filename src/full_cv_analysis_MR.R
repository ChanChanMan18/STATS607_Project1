#' Perform the cross-validation analysis in Moscovich and Rosset (2022)
#' 
#' Input: 
#' p - The number of predictors in the high-dimensional regression model.
#' K - The K covariates with the highest empirical variance
#' M - The number of predictors multiplied by a positive constant C.
#' C - Positive constant that multiplies first M predictors.
#' distribution - The mean-zero distribution from which the predictors are
#'                drawn. Inputs either "gaussian" or "t"
#' df - If distribution is "t", the number of degrees of freedom
#' eta - The scale of the noise error
#' sample_size - The sample sizes that are used to assess relative values of
#'               valuation error and risk. Should be vector object
#' cross_val - Two options: LOO - leave one out cross validation
#'                          2fold - 2-fold cross validation
#'
#' master_seed - The single seed used for reproducibility.
#' 
#' Output:
#' list object containing two lists:
#' mse_incorrect - for each sample size, the computed mse using the incorrect
#'                 data pipeline
#' mse_correct - for each sample size, the computed mse using the correct
#'               data pipeline
#' 
#' NOTE: Moscovich and Rosset use approximately 100,000 runs for each
#' sample size. Especially if using LOO CV, this is not advised without access
#' to high performance computing resources. Even 1,000 runs may take a very long
#' time if LOO CV is used.

# Loading necessary scripts
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
                                master_seed = NULL) {
  
  # Warning thrown if seed not supplied. One is provided in this case.
  if (is.null(master_seed)) {
    warning("Seed should be set for reproducibility. Setting seed to 82803...")
    Sys.sleep(1)
    set.seed(82803)
  } else {
    if (length(master_seed) > 1 ||
        !is.numeric(master_seed)) stop("master_seed must be a positive whole number")
    set.seed(master_seed)
  }
  
  # Check that num_runs is positive whole number
  if (length(num_runs) > 1 || 
      !is.numeric(num_runs) || 
      !(num_runs > 0) || 
      !(num_runs %% 1 == 0)) {
    stop("num_runs must be a positive whole number")
  }
  
  # Check if appropriate CV procedure provided by user
  allowed_cv <- c("2fold", "LOO")
  if (!is.character(cross_val) || 
      length(cross_val) != 1L ||
      !match(cross_val, allowed_cv, nomatch = 0L)) {
    stop("Distribution must be 'LOO' or '2fold'")
  }
  
  # Check sample_size dimensions and data types
  if ((length(sample_size) == 0) ||
       !is.numeric(sample_size)) {
    stop("sample_size must be numeric or vector of numerics")
  }
  
  # Check sample_size for duplicates
  if (!(length(sample_size) == length(unique(sample_size)))) {
    stop("sample_size must not contain duplicate values")
  }
  
  # Check sample_size is strictly increasing
  if (!all(diff(sample_size) > 0)) {
    stop("sample_size must have strictly increasing values")
  }
    
  # Precompute one unique seed per generate_sample call
  total_runs <- length(sample_size) * num_runs
  seed_stream <- sample.int(.Machine$integer.max,
                           total_runs,
                           replace = FALSE)
  
  # Initialize vectors of incorrect/correct MSE
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
      
      # Running 2-cross validations
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
        
        # Running LOO cross validation
      } else {
        
        # Computing MSE for Incorrect Pipeline, LOO
        mse_vec_incorrect[j] <- improper_cv_LOO(sample, 
                                            p,
                                            K,
                                            n) 
        
        # Computing MSE for Correct Pipeline, LOO
        mse_vec_correct[j] <- proper_cv_LOO(sample,
                                        p,
                                        K,
                                        n) 
      }
    }
    
    # Storing mean MSEs in output vectors
    mse_incorrect[n_idx] <- mean(mse_vec_incorrect)
    mse_correct[n_idx] <- mean(mse_vec_correct)
  }
  
  list(mse_incorrect = mse_incorrect,
       mse_correct = mse_correct)
}