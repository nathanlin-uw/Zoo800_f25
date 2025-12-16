# Quantitative Methods in Ecology and Evolution, Homework 13
# Nathan Lin
# November 25, 2025
# Maximum Likelihood Estimation

library(ggplot2)
library(dplyr)

## BACKGROUND ##
# We are seeing whether the dragon size (x) affects the acres burned (y)

# Load in the data
dragons <- read.csv("./HW 13/dragon_data.csv")

# Visualize
dragons_plot <- ggplot(dragons, aes(x=size, y=acres_on_fire)) + geom_point()
dragons_plot

# Rename to x and y for downstream things
dragons_renamed <- dragons %>% rename(x=size, y=acres_on_fire)

### Objective 1A - estimate parameters with analytical solution ###
# Make a matrix with an intercept column and the single predictor variable
data_matrix_X <- cbind(intercept=1, size=dragons_renamed$x)
# Pull out a vector of the response variable
results_vector_y <- dragons_renamed$y
# Calculate the analytical solution Beta = inv(XT * X) * XT * y
analytical_solution_betas <- solve(t(data_matrix_X) %*% data_matrix_X) %*% t(data_matrix_X) %*% results_vector_y

# Our analytical solution is -1.38 for intercept and 1.35 for slope
# Let's see it on the graph, sure checks out, nice
dragons_plot + geom_abline(slope=1.35, intercept=-1.38) + scale_x_continuous(limits=c(0, 60)) + scale_y_continuous(limits=c(0, 100))


### Objective 2A Ordinary Least Squares setup ###
# Set up our objective function first
# OLS tries to reduce the sum of squared residuals (SSR)
ssr_function <- function(parameters, data) {
  # Data should have an x column and a y column
  # parameters[1] is intercept, parameters[2] is slope
  with(data, sum((y - (parameters[1] + (parameters[2] * x))) ^ 2))
}

## Part a: using a grid search
# Set up our matrix for grid search (I'll do 30 slopes and 30 intercepts)
row_names_intercepts <- seq(from=-1.5, to=1.5, by=0.1)
column_names_slopes <- seq(from=-1.5, to=1.5, by=0.1)
grid_results <- matrix(nrow=length(row_names_intercepts), 
                       ncol=length(column_names_slopes), 
                       dimnames=list(row_names_intercepts, column_names_slopes))

# One for loop for intercept, one for loop nested inside for slope
for (intercept_index in seq(length(row_names_intercepts))) {
  for (slope_index in seq(length(column_names_slopes))) {
    intercept <- row_names_intercepts[intercept_index]
    slope <- column_names_slopes[slope_index]
    grid_results[intercept_index, slope_index] <- ssr_function(parameters=c(intercept, slope), 
                                                               data=dragons_renamed)
  }
}

# This pulls out the minimum SSR and the combination of intercept and slope for it
which(grid_results == min(grid_results), arr.ind=TRUE)
# Which turns out to be 0.2 intercept and 1.3 slope....
# I think this is different from the analytical solution because the sensitivity is not good enough
# There are smaller SSR values at different combinations if we go to the 0.xx place rather than just 0.x)

## Part b: using optim()
initial_parameters <- c(-1.38, 1.35)
# Run optim()
optim_result <- optim(par=initial_parameters, fn=ssr_function, data=dragons_renamed)
# See the result
optim_result
# Intercept of -1.373765, slope of 1.346651, value of 1042.508

## Part c: check convergence and starting value sensitivity
# Let's see if there is sensitivity to starting values
optim_other_start <- optim(par=c(500, 500), fn=ssr_function, data=dragons_renamed)
optim_other_start
# I got an intercept of -6.277586, slope of 1.490045, value of 1150.889
# It's a little different here maybe I started too far out
optim_other_start_extended <- optim(par=c(100, 100), fn=ssr_function, data=dragons_renamed)
optim_other_start_extended
# Here it is closer with parameters of (-1.553358, 1.355371) and value of 1043.662

# Convergence was indeed 0
# It is a little sensitive to starting values


### Objective 3A Maximum Likelihood Estimation setup ###
# Set up our objective function first
# MLE wants to minimize the negative log likelihood
neg_log_likelihood_function <- function(parameters, data) {
  # Data should have an x and y column
  # parameters[1] is intercept, parameters[2] is slope, parameters[3] is sigma_squared
  # MLE uses sigma_squared, the variance of the outcomes (y values)
  n <- nrow(data)
  ssr <- ssr_function(parameters=parameters, data=data)
  # We need the variance of the model's residuals?
  # I'll use the variance of the residuals from the previous part's regression
    # This variance is of the residuals which equals the sum of squared residuals divided by n
  sigma_squared <- ssr_function(parameters=c(-1.373765, 1.346651), data=dragons_renamed) / 50
    # Although why not just do the current ssr / n?
  ln_L <- ((-n / 2) * log(2 * pi * sigma_squared)) - ((1 / (2 * sigma_squared)) * ssr)
  # Negative log likelihood so we make lnL negative
  return(-1 * ln_L)
}

## Part a: using a grid search
# Set up our matrix for grid search (I'll do 30 slopes and 30 intercepts)
row_names_intercepts <- seq(from=-1.5, to=1.5, by=0.1)
column_names_slopes <- seq(from=-1.5, to=1.5, by=0.1)
grid_results_mle <- matrix(nrow=length(row_names_intercepts), 
                       ncol=length(column_names_slopes), 
                       dimnames=list(row_names_intercepts, column_names_slopes))

# One for loop for intercept, one for loop nested inside for slope
for (intercept_index in seq(length(row_names_intercepts))) {
  for (slope_index in seq(length(column_names_slopes))) {
    intercept <- row_names_intercepts[intercept_index]
    slope <- column_names_slopes[slope_index]
    grid_results_mle[intercept_index, slope_index] <- neg_log_likelihood_function(parameters=c(intercept, slope, sigma_squared), 
                                                               data=dragons_renamed)
  }
}

# This pulls out the minimum negative log likelihood and the combination of intercept and slope for it
which(grid_results_mle == min(grid_results_mle), arr.ind=TRUE)
# Which turns out to be 0.2 intercept and 1.3 slope again

## Part b: using optim()
initial_parameters <- c(-1.38, 1.35)
# Run optim()
optim_result_mle <- optim(par=initial_parameters, fn=neg_log_likelihood_function, data=dragons_renamed)
# See the result
optim_result_mle
# Intercept of -1.379885, slope of 1.346827, value of 146.881

## Part c: check convergence and starting value sensitivity
# Let's see if there is sensitivity to starting values
optim_other_start_mle <- optim(par=c(500, 500), fn=neg_log_likelihood_function, data=dragons_renamed)
optim_other_start_mle
# I actually got something similar to the SSR with intercept of -6.277586, slope of 1.490045, value of 149.48
# It's a little different here maybe I started too far out
optim_other_start_extended <- optim(par=c(100, 100), fn=neg_log_likelihood_function, data=dragons_renamed)
optim_other_start_extended
# Here it is closer with parameters of (-1.553358, 1.355371) and value of 146.9086

# Convergence was indeed 0
# MLE is actually as sensitive to the starting value
  # But if we calculate the residuals for every new residual pair it becomes a lot less sensitive to the starting value?
  # eg do ssr(current parameters being tested)/n instead of ssr(previous regression's parameters)/n


### Objective 4 - comparisons ###
# The slope estimates for grid search, optim, and the analytical solution were very similar
# HOWEVER the intercept was very different for grid search, likely because we were only going by 0.1 increments
# Using least squares versus max likelihood did not have much of a difference, but maybe that was
  # because we used the optimal parameters for the least squares regression for log likelihood variance??


########## Notes from class #############
# Likelihood includes variance sigma^2 -- we have to either search for an additional parameter or add max likelihood estimation of the variance
# that would be variance of the residuals
# plug that in from the answer to objective 2
# Really search for 3 parameters the slope intercept and variance -- but put in what we have for variance of residuals from objective 2's regression