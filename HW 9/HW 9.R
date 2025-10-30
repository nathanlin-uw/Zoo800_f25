# Quantitative Methods in Ecology and Evolution, Homework 9
# Nathan Lin
# October 30, 2025
# Probability Distributions

library(dplyr)
library(ggplot2)
library(tidyr)
library(here)

## Question 1a - generating observations for x and y
# I'll make a function for this since we simulate with different sigma values later
generate_xy_observations <- function(alpha_intercept, beta_slope, sigma_stdev) {
  # Generates x and y observations for a simple linear regression. 
  
  # Pull a hundred random x values from a uniform distribution 0 to 10
  one_hundred_random_x <- runif(100, min=0, max=10)
  
  # Generate a hundred random errors from a normal distribution with mean 0 and SD of sigma
  one_hundred_errors <- rnorm(100, mean=0, sd=sigma_stdev)
  
  # Calculate y based on x, slope, intercept, and error
  one_hundred_calculated_y <- alpha_intercept + (beta_slope * one_hundred_random_x) + one_hundred_errors
    
  # Make a dataframe with observations
  x_y_error_df <- data.frame(x=one_hundred_random_x, y=one_hundred_calculated_y, error=one_hundred_errors)
  
  return(x_y_error_df)
}

# Testing, I'll just use these values for intercept and slope 
  # since they keep the y between -100 and 100 when sd is 0
# If I wanted to keep values between -100 and 100 for all three sd's 1, 10, and 25 
  # I would need to change those parameters and range of sd=1 y's would be restricted
observations <- generate_xy_observations(alpha_intercept=-100, beta_slope=20, sigma_stdev=0)
range(observations$y)

## Question 1b - ggplotting the different sigmas
# I realize I need to make it all in one table for ggplot...
# I'll scrap the function idea then

# Pull a hundred random x values from a uniform distribution 0 to 10
one_hundred_random_x <- runif(100, min=0, max=10)

# Calculate y based on x, slope, intercept, and error
alpha_intercept = -100
beta_slope = 20
one_hundred_y_no_error <- alpha_intercept + (beta_slope * one_hundred_random_x)

# Generate a hundred random errors from a normal distribution with mean 0 and SD of different sigmas
one_hundred_errors_sd_1 <- rnorm(100, mean=0, sd=1)
one_hundred_errors_sd_10 <- rnorm(100, mean=0, sd=10)
one_hundred_errors_sd_25 <- rnorm(100, mean=0, sd=25)

# Make a dataframe with observations
x_y_error_three_sigmas_df <- data.frame(x = one_hundred_random_x, 
                           error_sd_1 = one_hundred_errors_sd_1,
                           error_sd_10 = one_hundred_errors_sd_10,
                           errors_sd_25 = one_hundred_errors_sd_25,
                           y_sigma_1 = one_hundred_y_no_error + one_hundred_errors_sd_1,
                           y_sigma_10 = one_hundred_y_no_error + one_hundred_errors_sd_10,
                           y_sigma_25 = one_hundred_y_no_error + one_hundred_errors_sd_25)

# Turn that into a ggplottable table
for_ggplot_x_y_three_sigmas <- x_y_error_three_sigmas_df %>%
  pivot_longer(cols=!contains(c("x", "error")))

# Plot it 
sigma_comparison_plot <- ggplot(for_ggplot_x_y_three_sigmas, aes(x=x, y=value)) + 
  geom_point() +
  labs(y="y") + theme_bw() +
  facet_wrap(~name, labeller=labeller(name=c(y_sigma_1 = "Sigma = 1", 
                                        y_sigma_10 = "Sigma = 10", 
                                        y_sigma_25 = "Sigma = 25")))
# See the plot
sigma_comparison_plot

## Question 1c 
# As observation error increases it looks like it's harder to visually detect 
  # a relationship between x and y

## Question 2a 
# We are looking at 1 to 20 coin flips
# For each number of coin flips we are trying to find the probability that we'll
  # find the coin is significantly unfair (alpha < 0.05) 
# So we will need to simulate coin flips with N=that 1-20 and p=0.55
# We will do a hypothesis test for is the probability of seeing the number 
  # of heads we saw less than 0.05 (if we have a normal coin with p=0.5)
# Do this 100 times, count up the number of times we saw significant results
# Then store everything in a table where x is the number of coin flips
  # and y is the number of significant detections
# Then we make a line plot with this

one_hundred_coinflip_evaluations <- function(how_many_flips, fair_null_p, actual_p, alpha) {
  # This function returns the number of hypothesis tests out of 100 simulations
    # That resulted in a significant detection of coin unfairness
  
  # Count this up every time we find an unfair coin
  unfair_detections <- 0
  
  # Just for me to make sure it makes sense
  print(paste("---------", how_many_flips, "coin flips ------------"))
  
  # Simulate however many coin tosses 100 times
  for (i in 1:100) {
    # See how many heads we get with our p
    how_many_heads <- rbinom(1, size=how_many_flips, actual_p)
    
    # Probability of that happening under a fair coin
    # Aka probability of exactly this many heads in however many trials with 0.5 probability
    result_probability_with_fair_coin <- dbinom(x=how_many_heads, size=how_many_flips, prob=fair_null_p)
    
    # If we caught an unfair coin update unfair_detections
    if (result_probability_with_fair_coin < alpha) {
      # These prints are just for me to make sure it makes sense
      print(paste("number of heads: ", how_many_heads))
      print(paste("unfair detection", result_probability_with_fair_coin))
      unfair_detections = unfair_detections + 1
    }
  }
  
  # Return how many detections we had
  return(unfair_detections)
}


get_detection_probabilities <- function(starting_flips, ending_flips, fair_null_p, alpha, actual_p) {
  # This function returns a dataframe containing the probabilities of detecting an unfair coin for a range of flip counts
  # Make a storage dataframe
  detections_probability_df <- data.frame(number_of_coinflips=starting_flips:ending_flips, unfair_detections_probability=0)
  
  # Go through however many coinflips we wanted
  for (i in starting_flips:ending_flips) {
    # Run the simulation
    how_many_detections <- one_hundred_coinflip_evaluations(how_many_flips=i, fair_null_p=fair_null_p, actual_p=actual_p, alpha=alpha)
    
    # Store the results in the dataframe
    detections_probability_df[i, "unfair_detections_probability"] <- how_many_detections / 100
  }
  
  return(detections_probability_df)
}

coinflip_results_p_0.55 <- get_detection_probabilities(starting_flips=1,
                                                       ending_flips=20,
                                                       fair_null_p=0.5,
                                                       alpha=0.05,
                                                       actual_p=0.55)

# Plot this
results_plot <- coinflip_results_p_0.55 %>% 
  ggplot(aes(x=number_of_coinflips, y=unfair_detections_probability)) +
  geom_line() + geom_point() +
  theme_bw()

# Preview plot
results_plot

## Question 2b 
# Simulate for both p=0.6 and p=0.65
coinflip_results_p_0.6 <- get_detection_probabilities(1, 20, 0.5, 0.05, actual_p=0.6)
coinflip_results_p_0.65 <- get_detection_probabilities(1, 20, 0.5, 0.05, actual_p=0.65)

# I need to merge the tables using number_of_coinflips as a join key
combined_coinflip_results <- merge(coinflip_results_p_0.55, 
                                   coinflip_results_p_0.6,
                                   by="number_of_coinflips",
                                   suffixes=c("_p_0.55", "_p_0.6")) %>% 
  merge(coinflip_results_p_0.65,
        by="number_of_coinflips") %>% 
  rename(unfair_detections_probability_p_0.65=unfair_detections_probability)

# Then I'll put them into a ggplottable format with pivot_longer
for_ggplot_coinflip_results <- combined_coinflip_results %>%
  pivot_longer(cols=!contains("number_of_coinflips"))

# New plot with all three lines
new_results_plot <- for_ggplot_coinflip_results %>% 
  ggplot(aes(x=number_of_coinflips, y=value, col=name)) + 
  geom_line() + 
  labs(x="Number of coinflips", y="Unfairness detection probability", color="Degree of unfairness") +
  scale_color_discrete(labels=c("p=0.55", "p=0.6", "p=0.65")) +
  theme_bw()

# Preview the plot
new_results_plot

# Save the plots
setwd("./HW 9")
ggsave(plot=sigma_comparison_plot, filename="./sigma_comparisons.png", width=12, height=7, dpi=300)
ggsave(plot=new_results_plot, filename="./unfairness_evaluations.png", width=12, height=7, dpi=300)
