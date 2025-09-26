# Quantitative Methods in Ecology and Evolution, Homework 3
# Nathan Lin and Nurul Islam
# September 18, 2025
# Coding in R 1: syntax, variables, arrays, and dataframes; for loops and apply

library(glue)
library(dplyr)
library(LakeMetabolizer)

# Question 1.1 - Make our variable for Celsius
temp_C <- 18.5

# Question 1.2 - Celsius to Fahrenheit conversion is F = (C * (9/5)) + 32
temp_F <- (temp_C * (9/5)) + 32

# Question 1.3 - Printing fiasco (I was missing f strings on python)
print(glue("The water temperature is {temp_C} °C ({temp_F} °F)"))
  # print(paste("The water temperature is ", temp_C, " °C (", temp_F, " °F)", sep=""))
    # paste works nicely for saving files
  # cat(yada yada) works as well too
  # could've done expression(degrees) to have that degree symbol -- this is useful for things like 
    # greek letters delta/Sigma or subscripting N[0] like with expression(Delta == 1 (i.e., N[0]))

# Question 2.1 - Make species counts (it's a named vector, name=value)
species_counts <- c(Bluegill = 12, Bass = 7, Sunfish = 21, Carp = 3)

# Question 2.2
# Find fish total
fish_total <- sum(species_counts)
# Find species with highest count (reorder then pull names then pull first VS just pull names of max)
most_common_species <- names(species_counts[order(species_counts, decreasing=TRUE)])[1]
  # most_common_species <- names(species_counts[which.max(species_counts)])

# Question 2.3 - Make chlorophyll concentrations matrix 3x3 array (in μg/L)
chlorophyll_matrix <- array(data=c(c(13, 7, 21), c(4, 8, 48), c(15, 18, 25)), dim=c(3,3), dimnames=(list(c("surface", "mid", "bottom"), c("Monday", "Wednesday", "Friday"))))

# Question 2.4 - Calculate average for each depth across days
average_per_depth_across_days <- rowMeans(chlorophyll_matrix)
# surface_average <- chlorophyll_matrix["surface", ]
# mid_average <- chlorophyll_matrix["mid", ]
# bottom_average <- chlorophyll_matrix["bottom", ]

# Question 3.1 - Making lakes dataframe
lakes <- data.frame(Temp_C=c(22.4, 25.1, 23.7, 24.6, 26.0), DO_mgL=c(8.3, 6.7, 7.5, 7.9, 6.2), row.names=c("Mendota", "Wingra", "Monona", "Waubesa", "Kegonsa"))

# Question 3.2 - Calculating mean of each variable across lakes
mean_temp_and_d_o <- lakes %>% apply(MARGIN=2, FUN=mean)

# Question 3.3 - Adding column with Fahrenheit
lakes_with_fahrenheit <- lakes %>% mutate(Temp_F=((Temp_C * (9/5)) + 32))
  # lakes$Temp_F <- (Temp_C * (9/5)) + 32

# Question 3.4 - Dissolved oxygen with LakeMetabolizer
# Adding column with equilibrium dissolved oxygen (just according to temp)
with_eq_d_o <- lakes_with_fahrenheit %>% mutate(Eq_DO=o2.at.sat.base(temp=Temp_C))
# Adding column of dissolved oxygen % saturation (current / max)
with_percent_d_o_saturation <- with_eq_d_o %>% mutate(Percent_Sat_DO=DO_mgL/Eq_DO)
# Order (decreasing) according to dissolved oxygen saturation 
ordered_lakes <- with_percent_d_o_saturation[order(with_percent_d_o_saturation$Percent_Sat_DO, decreasing=TRUE), ]

# Question 4.1 - # Square of each number 1 to 10
for (i in 1:10) {
  print(i ** 2)
}

# Question 4.2 - Modeling exponential population growth
# Set up parameters
N0 <- 10
r <- 0.3
t <- 10
# Storage vector for pop sizes
pop <- character(t)
# Calculate pop sizes
for (i in 1:t) {
  Nt <- N0 * exp(r * i) 
  pop[i] <- Nt
  # pop <- c(pop, Nt) # note don't use this if you need to name the 2nd thing
}

# Question 4.3 - Making phosphorus list
phosphorus <- list(Lake1=c(6, 8, 10, 12), Lake2=c(10, 20, 30, 40), 
                   Lake3=c(5, 7, 9, 11), Lake4=c(4, 8, 16, 24),
                   Lake5=c(9, 16, 25, 36))

# Question 4.4 - Calculating and reporting lake mean phosphorus concentrations
# Make storage vector
lake_means <- c()
for (lake in names(phosphorus)) {
  mean_phosphorus <- mean(phosphorus[[lake]])
  lake_means[lake] <- mean_phosphorus
  print(glue("{lake} mean phosphorus = {mean_phosphorus} μg/L"))
}

# Question 4.5 - Printing the lake means
print(lake_means)

# Question 5.1 - Back to chlorophyll array
# Calculate mean concentration for each depth (rows)
average_per_depth_across_days <- apply(chlorophyll_matrix, MARGIN=1, FUN=mean)
# Calculate mean concentration for each day (columns)
average_per_day_across_depths <- apply(chlorophyll_matrix, MARGIN=2, FUN=mean)

# Question 5.2 - Back to lakes
column_range_across_lakes <- apply(ordered_lakes, MARGIN=2, FUN=range)

# Question 5.3 - Back to population growth
pop_growth_with_sapply <- sapply(X=(1:10), FUN=function(i) (N0 * exp(r * i)))
  # Using sapply feels so much cleaner actually wow!