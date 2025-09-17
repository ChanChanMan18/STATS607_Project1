#' Perform preprocessing according to main example (Section 4)
#' of Moscovich and Rosset (2002). In particular, the authors
#' use an unsupervised variance-based feature selection on the
#' entire dataset. Note that this is an erroneous application
#' of feature selection used to illustrate the main idea of the
#' paper.
#' 
#' Input:
#' X - design matrix whose covariates are those produced by 
#'     generate_sample.R
#' K - Select the K covariates with the highest empirical
#'     variance
#'     
#' Output:
#' output - a List object containing two elements
#' X_K_highest_var - Design matrix consisting of the K covariates with
#'                   the largest empirical variance
#' highest_var_ID - indices of those covariates with the largest empirical
#'                  variance *from the original design matrix* X

feature_select_var <- function(X, K) {
  
  col_vars <- apply(X, 2, var)
  highest_var <- order(col_vars,
                      decreasing = TRUE)[1:K]
  
  # Selecting top K columns with largest variance
  X_K_highest_var <- X[, highest_var, drop=FALSE]
  
  output <- list(X_K_highest_var = X_K_highest_var,
                      highest_var_ID = highest_var)
  
  return(output)

}