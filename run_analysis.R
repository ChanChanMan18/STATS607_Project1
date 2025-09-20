#' Run the entire pipeline to reproduce the main result of Moscovich and Rosset
#' (2022). There are suggested initial parameters for the user
#' such that the code runs relatively quickly and reproduces a result
#' from the paper. Note that if cross_val = "LOO", the code may take a very
#' long time to run without access to high-performance computing.
#' The definitions of these parameters are provided below; for
#' more information, read the work "On the cross-validation bias due to 
#' unsupervised preprocessing", particularly section 4.
#' 
#' This was completed for the final project of STATS601 - Advanced Statistical
#' Learning and refactored for STATS607 - Advanced Statistical Computing, both
#' course offerings at the University of Michigan.
#' 
#' num_runs - The number of iterations of the entire experiment for each sample size. 
#'            These runs are averaged to produce a single MSE (mean squared error) 
#'            using an incorrect cross-validation pipeline and a correct
#'            cross-validation pipeline.
#'            
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
#' NOTE: Moscovich and Rosset use approximately 100,000 runs for each
#' sample size (and thus error bars are excluded). Especially if using LOO CV, 
#' this is not advised without access to high performance computing resources. 
#' Even 1,000 runs may take a very long time if LOO CV is used.
     
### For testing time taken to run script ###
start_time <- Sys.time()
   
source('src/full_cv_analysis_MR.R')

### Suggested Initial Parameters ###
num_runs <- 500
p <- 100
K <- 10
M <- 5
C <- 5
distribution <- "gaussian"
df <- 4
eta <- 1
sample_size <- seq(25, 60, 5)
cross_val <- "2fold"

### Setting master_seed for reproducibility ###
master_seed <- 82803

### Run analysis ###
results <- full_cv_analysis_MR(p = p,
                               K = K,
                               M = M,
                               C = C,
                               distribution = distribution,
                               df = df,
                               eta = eta,
                               num_runs = num_runs,
                               sample_size = sample_size,
                               cross_val = cross_val,
                               master_seed = master_seed)

### Save Example to simulated_data/ ###
example_dat <- generate_sample(max(sample_size), p, M, C, 
                               distribution, df, eta, master_seed)
folder_path <- "simulated_data/"
file_name <- paste0("n", max(sample_size), "p", p, "K", K, "M", M, "C", C, 
                    "dist_", distribution, "seed", master_seed, ".RData")
full_path <- file.path(folder_path, file_name)
save(example_dat, file = full_path)

### Plot results ###
x <- sample_size
y1 <- results$mse_correct
y2 <- results$mse_incorrect

plot_title <- paste0('MSE versus Sample Size, p = ', p, ", M = ", M, ", C = ", C,
                     ", K = ", K, ", generating distribution = ", distribution,
                     ", cross-val = ", cross_val)

PNG_name <- paste0('p=', p, "--M=", M, "--C=", C,
                     "--K=", K, "--generating_distribution=", distribution,
                   "--cross-val=", cross_val)

### Open png device to save to results folder ###
png(paste0("results/figures/", PNG_name, ".png"), width = 800, height = 600)

plot(x, y1, 
     type = 'l', 
     col = 'orange', 
     ylim = c(min(y1, y2), max(y1, y2)), 
     xlab = 'n', 
     ylab = 'MSE', 
     main = plot_title)

lines(x, 
      y2, 
      col = 'blue')

legend('topright', 
       legend = c('Valuation Error', 'Risk'), 
       col = c('blue', 'orange'), 
       lty = 1)

print("Your new figure was saved to results/figures/")
end_time <- Sys.time()
time_taken <- as.numeric(end_time - start_time, units = "secs")
minutes <- floor(time_taken / 60)
seconds <- time_taken %% 60

print(paste0("Your analysis took ", round(minutes, 1), " minute(s) and ", round(seconds, 2), " seconds to run"))

### Close the png device ###
dev.off()

