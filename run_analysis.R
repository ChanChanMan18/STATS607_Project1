#' Run the entire pipeline to reproduce the main result of Moscovich and Rosset
#' (2022). Note that this code may run for quite some time since it is running
#' LOO cross-validation. There are suggested initial parameters for the user
#' such that the code runs relatively quickly and reproduces a result
#' from the paper. The definitions of these parameters are provided below; for
#' more information, read the work "On the cross-validation bias due to 
#' unsupervised preprocessing", particularly section 4.
#' 
#' This was completed for the final project of STATS601 - Advanced Statistical
#' Learning at the University of Michigan
#' 
#' num_runs - The number of iterations of the entire experiment. These runs are 
#'            averaged to produce a single MSE (mean squared error) using an
#'            incorrect cross-validation pipeline and a correct cross-validation
#'            pipeline.
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
#'
#' master_seed - The single seed used for reproducibility.
        
source('src/full_cv_analysis_MR.R')

### Suggested Initial Parameters ###
num_runs <- 50
p <- 100
K <- 10
M <- 5
C <- 5
distribution = "gaussian"
df <- 4
eta <- 1
sample_size <- seq(200, 400, 50)


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
                               sample_size = sample_size,
                               master_seed = master_seed)


### Plot results ###
x <- sample_size
y1 <- results$mse_correct
y2 <- results$mse_incorrect

plot_title <- paste0('MSE versus Sample Size, p = ', p, ", M = ", M, ", C = ", C,
                     ", K = ", K, ", generating distribution = ", distribution)

PNG_name <- paste0('p=', p, ", M=", M, ", C=", C,
                     ", K=", K, ", generating distribution=", distribution)

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


dev.off()

