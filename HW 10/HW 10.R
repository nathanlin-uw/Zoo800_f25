# Quantitative Methods in Ecology and Evolution, Homework 10
# Nathan Lin, Kristine Schoenecker, Elizabeth Braatz
# November 6, 2025
# Linear Regression

library(readxl)
library(dplyr)
library(ggplot2)
library(ggfortify)

set.seed(1234)

## Objective 1A: Get those paired observations with an X and a Y

# Using data from Daniel et al. (2021) Thermal flight performance reveals impact of warming on bumblebee foraging potential
# https://datadryad.org/dataset/doi:10.5061/dryad.18931zcxr#citations

# We will be looking at weight out of the nest as the predictor variable x, 
# and weight after flight as the response variable y.

# Load in data
bee_data <- read_xlsx("./HW 10/Data_Full_Overview.xlsx")

# Pull out relevant columns and remove NA
cleaned_bees <- bee_data %>% mutate(weight_end=as.numeric(bee_data$"Weight of bee after flight (g)")) %>%
                              select(bee_id= "Bee-ID", 
                              weight_start="Weight out of nest (g)", 
                              weight_end) %>% na.omit()

# See what our data looks like, pretty linear relationship
ggplot(cleaned_bees, aes(x=weight_start, y=weight_end)) + geom_point()
# The starting weight looks pretty nicely close to normally distributed
ggplot(cleaned_bees, aes(x=weight_start)) + geom_histogram()
# The ending weight also looks close to normal 
ggplot(cleaned_bees, aes(x=weight_end)) + geom_histogram()

## Objective 1B: fit a linear regression
# Make the lm object
lm_object <- lm(weight_end ~ weight_start, data=cleaned_bees)
# Looks like the line is y = 0.801x + 0.036, R^2 of 0.6315
summary(lm_object)
# Plot a regression
ggplot(cleaned_bees, aes(x=weight_start, y=weight_end)) + geom_point() + geom_smooth(method="lm", )

## Objective 1C: check for violations of our assumptions
autoplot(lm_object)
# Residuals vs Fitted Values plot
  # It looks like there are a few points with really large residuals, with most of them
    # corresponding to average or higher fitted values (the lower ones are generally pretty uniform).
  # There is no clear pattern to the residuals, although there is a group of large ones at fitted values of around 0.25.
# Normal QQplot 
  # This follows a normal distribution pretty solidly most of the time (between -2 and 2 of the theoretical quantiles).
  # Past those areas, it starts to deviate quite a bit.
# Residuals vs Leverage plot
  # There are a good deal of large residuals with little to no leverage, and some larger residuals with 
    # lower mid to high leverage. There are a few low-residuals points that have much more leverage than other points,
    # and there is a single point with a large residual and high leverage.

# Assumption number 1: linearity
# We want to check that the response and predictor variables have a linear relationship
ggplot(cleaned_bees, aes(x=weight_start, y=weight_end)) + geom_point()
# This plot of response vs predictor variable shows us that there is a pretty linear relationship.
# There are fewer data points at higher starting weights and those are more spread out,
  # so I'm not sure if the linearity holds up at those higher values, but for the most part it seems solid.

# Assumption number 2: normal distribution of errors
# We want to check that the errors are normally distributed
autoplot(lm_object)
# From the Normal Q-Q plot, we can see that the residuals do not align with expected values
  # for a normal distribution beyond the -2 and 2 theoretical quantiles. At those more
  # extreme ranges, we see large deviations from a normal distribution, which violates
  # this linear model assumption.

# Assumption number 3: homoscedasticity
# We want to check that the variance of the residuals stays constant throughout
autoplot(lm_object)
# Based on the residuals vs fitted values graph, we would say this is violated because
  # as fitted values increase there is more variance in the residuals.
# aka the points are a lot tighter to the 0 residuals line at lower fitted values.


## Objective 1D: predictions and prediction intervals 
# For the median value of X
# Pull out median value
start_median <- median(cleaned_bees$weight_start)
# Generate prediction using our lm
predict.lm(lm_object, data.frame(weight_start=start_median), interval="prediction")
# Our fitted value is 0.221, with prediction interval [0.1664, 0.2747]

# For the 95th percentile of X
# Pull out 95th percentile
start_95th <- quantile(x=cleaned_bees$weight_start, probs=(0.95))
# Generate prediction using our lm
predict.lm(lm_object, data.frame(weight_start=start_95th), interval="prediction")
# Our fitted value is 0.282, with prediction interval [0.2275, 0.3362]

# The prediction intervals are around the same size, but the 95th percentile one has larger values
  # This makes sense because it's farther to the right of the graph and it's a positive association


## Objective 2A: making linear regression data without normally-distributed Y
# I'll use negative binomial, we make 100 xy pairs with error
# For x values I'll do a standard normal
sim_x_values <- rnorm(100)
# Set the slope and intercept
true_slope <- 4
true_intercept <- 8
# For error values I'll do a standard lognormal
sim_error_values_lnorm <- rlnorm(100)
# See what the y values look like, yeah it's very much not normal
hist(sim_error_values_lnorm)

# Calculate the y values from everything
sim_y_values <- true_intercept + (true_slope * sim_x_values) + sim_error_values_lnorm

# Just visualize, yeah it's linear
ggplot(mapping=aes(x=sim_x_values, y=sim_y_values)) + geom_point()

## Objective 2B: fit a linear model
sim_lm_object <- lm(sim_y_values ~ sim_x_values)
sim_lm_object

## Objective 2C: unsure what to be repeating but i'll pull out slope and intercept
lm_slope <- sim_lm_object$coefficients["sim_x_values"] 
lm_intercept <- sim_lm_object$coefficients["(Intercept)"]

# In case this is asking to repeat but with normally-distributed errors I'll do that
sim_error_values_norm <- rnorm(100)
sim_y_values_norm <- true_intercept + (true_slope * sim_x_values) + sim_error_values_norm
sim_lm_norm <- lm(sim_y_values_norm ~ sim_x_values)
sim_lm_norm

lm_slope_norm <- sim_lm_norm$coefficients["sim_x_values"]
lm_intercept_norm <- sim_lm_norm$coefficients["(Intercept)"]

## Objective 2D: how do the true and estimated slopes and intercepts compare?
# For the lnorm errors:
# The true slope is 4, while the calculated slope was 3.672. Not perfect but not bad!
# The true intercept is 8, while the calculated intercept was 9.512. It's close!

# For the norm errors:
# The calculated slope and intercept were 3.916 and 7.919. Much better here!

## Objective 2E: 95% prediction intervals for each x value
# For the lnorm errors simulation:
lnorm_sim_95_pred_intervals <- predict.lm(sim_lm_object, data.frame(x=sim_x_values), interval="prediction")

# For the norm errors simulation:
norm_sim_95_pred_intervals <- predict.lm(sim_lm_norm, data.frame(x=sim_x_values), interval="prediction")

## Objective 2F: what fraction falls in there?
# For lnorm errors simulation: 95 out of 100 y values fell in the prediction interval
sum((sim_y_values <= lnorm_sim_95_pred_intervals[, 3] & sim_y_values >= lnorm_sim_95_pred_intervals[, 2]))

# For norm errors simulation: 96 out of 100 y values fell in the prediction interval
sum((sim_y_values_norm <= norm_sim_95_pred_intervals[, 3] & sim_y_values_norm >= norm_sim_95_pred_intervals[, 2]))

## Objective 2G: implications in estimated uncertainty vs true uncertainty
# Perhaps estimates of uncertainty are a little worse for log normal than normal,
  # but I can't say by how much. It actually seemed like the parameter estimation was
  # worse than the prediction interval performance here, but I'm not sure how much this
  # depends on the given slope and intercept and error and x distributions.