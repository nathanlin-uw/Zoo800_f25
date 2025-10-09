# Quantitative Methods in Ecology and Evolution, Homework 6
# Nathan Lin, Evan Peepo, and Jillian Neece
# October 9, 2025
# Coding in R 4: ggplot

# Initial setup
library(ggplot2)

# For the sake of graph consistency later this controls the RNG
set.seed(4)

hellbender_mean_r <- 0.2
hellbender_K <- 1000
hellbender_N0 <- 50

### Objective 1
# Question 1a - simulate the population growth
sim_log_growth <- function(how_many_years, N0_starting_pop_size, r_growth_rate, K_carrying_capacity) {
  # Returns a vector of 11 values for initial population size + size over the next however many years
  # Make our pop sizes vector (make sure it's numeric not character otherwise the numbers will be characters)
  pop_sizes <- c()
  # Stick initial pop size in there first
  pop_sizes[1] <- N0_starting_pop_size
  # We can also just do i-1 in the for loop and not need this
  previous_pop_size <- N0_starting_pop_size
  # For loop for the next 10 years
  for (i in 2:(how_many_years + 1)){
    next_pop_size <- previous_pop_size + (r_growth_rate * previous_pop_size * (1 - (previous_pop_size / K_carrying_capacity)))
    pop_sizes[i] <- next_pop_size
    previous_pop_size <- next_pop_size
  }
  return(pop_sizes)
}

ten_year_hellbenders <- sim_log_growth(10, hellbender_N0, hellbender_mean_r, hellbender_K)

# Question 1b - plot the time series
hellbenders_df <- data.frame(year=0:10, pop_size=ten_year_hellbenders)
hellbenders_10_year_graph <- ggplot(data=hellbenders_df, mapping=aes(x=year, y=pop_size)) + geom_line()
hellbenders_10_year_graph

### Objective 2
# Question 2a - simulate 50 different times using normal distribution of r (mean=0.2, sd=0.03)
different_r_values <- rnorm(50, mean=0.2, sd=0.03)
simulate_50_times <- list()
for (i in 1:50) {
  one_simulation <- sim_log_growth(10, hellbender_N0, different_r_values[i], hellbender_K)
  simulate_50_times[[i]] <- one_simulation
}

# We're going to have column names for each extra simulation
extra_sim_names <- paste("extra_sim_", 1:50, sep="")
names(simulate_50_times) <- extra_sim_names
simulate_50_times

# Question 2b - graph it 
library(dplyr)
library(tidyr)

# Made our 50 simulation values a wide dataframe
df_50_sims <- do.call(cbind.data.frame, simulate_50_times) %>% mutate(year=0:10)

# Join the two dataframes
new_hellbenders_df <- left_join(hellbenders_df, df_50_sims, by=join_by(year))

# ggplot prefers long format than wide
hellbenders_long_format <- new_hellbenders_df %>% pivot_longer(cols=!contains("year"), names_to="simulation", values_to="population_size")

# Graph it at last
hellbenders_possible_time_series <- ggplot(hellbenders_long_format, aes(x=year, y=population_size, color=simulation)) + 
  geom_line(linewidth=1) + theme_bw() + theme(legend.position="none")
hellbenders_possible_time_series

# Try out geom_ribbon with a min and max


### Objective 3
# Question 3a - horizontal dashed line for target size
hellbenders_possible_time_series + geom_hline(yintercept=800, linetype="dashed")

# Question 3b - histogram for 25 years from now
# Just making a function since I like organizing my work into those
sim_log_growth_50_times <- function() {
  simulation_endpoints <- numeric(50)
  for (i in 1:50) {
    sim_values <- sim_log_growth(how_many_years=25, N0_starting_pop_size=50, r_growth_rate=different_r_values[i], K_carrying_capacity=hellbender_K)
    endpoint <- sim_values[26]
    simulation_endpoints[i] <- endpoint
  }
  return(simulation_endpoints)
}

# Run the simulation
simulation_endpoints_25_years <- sim_log_growth_50_times()

# Plotting with red line
pop_sizes_in_25_years <- ggplot() + geom_histogram(aes(simulation_endpoints_25_years), binwidth=20) + geom_vline(colour="red", linewidth=2, xintercept=800) + labs(x="population sizes after 25 years")
pop_sizes_in_25_years

# Question 3c - what fraction meets that target
# Calculate percent
what_percent <- sum(simulation_endpoints_25_years >= 800) / 50 * 100
# Add an annotation
annotated_pop_sizes_25_years <- pop_sizes_in_25_years + annotate("text", x=700, y=6, label=paste("~", what_percent, "% of trajectories \nhit the target population size.", sep="")) + labs(y="count")
annotated_pop_sizes_25_years
