library(tidyverse)


# Q1 ----------------------------------------------------------------------


log_pop_model <- function(starting_size, num_years, K, r) {
  pop_values = c(starting_size)
  current_value <- starting_size
  for (i in 1:num_years) {
    current_value <- current_value + r * current_value * (1 - current_value / K)
    pop_values <- c(pop_values, current_value)
  }
  return(pop_values)
}
  
ten_yr_growth <- log_pop_model(starting_size = 50, num_years = 10, K = 1000, r = 0.2)
years <- c(0:10)

pop_df <- data.frame(years, ten_yr_growth)
names(pop_df) <- c('year', 'population_size')

ggplot(pop_df, aes(x = year, y = population_size)) +
  geom_point() +
  labs(
    x = 'years',
    y = "population size"
  ) +
  theme_minimal(base_size = 14)


# Q2 ----------------------------------------------------------------------

r_vec <- rnorm(50, mean = 0.2, sd = 0.03)

sim_50 <- list()

for (i in r_vec) {
  x <- log_pop_model(starting_size = 50, num_years = 10, K = 1000, r = i)
  sim_50[[length(sim_50) + 1]] <- x
}

sim_50_df <- do.call(cbind.data.frame, sim_50)
names(sim_50_df) <- c(paste0('sim' , 1:50))
years_df <- data.frame(years)
sim_50_df <- cbind(sim_50_df, years_df)

sim_50_df_long <- sim_50_df %>% 
  pivot_longer(
    cols = starts_with('sim'),
    names_to = 'simulation' ,
    values_to = 'population_size'
  )

sim_50_summary <- sim_50_df_long %>% 
  group_by(years) %>% 
  summarise(
    min = min(population_size),
    max = max(population_size)
  )

ggplot(sim_50_df_long, aes(x = years, y = population_size)) +
  geom_point(alpha = 0.1) +
  geom_smooth(color = "black") +
  geom_ribbon(data = sim_50_summary, alpha = 0.4, aes(x = years, ymin = min, ymax = max), 
              inherit.aes = FALSE) +
  labs(
    x = 'years',
    y = "population size"
  ) +
  theme_minimal(base_size = 14) +
  theme(legend.position = 'none')
