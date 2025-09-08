# Quantitative Methods in Ecology and Evolution, Homework 1
# Nathan Lin

# Get working directory sorted out
setwd("~/Zoo800_f25/HW 1/")

# Problem setup
K <- 1000
r <- 0.0005
N0 <- 1
start_year <- 1950


### Objective 1 ###
# I start with a function to calculate Nt
calculate_Nt <- function(K, N0, r, end_year) {
  t <- end_year - start_year
  return(K / ((((K - N0) / N0) * exp(-1 * r * K * t)) + 1))
}

# Get a vector of the years
from_1950_to_2025 <- seq(1950, 2025)
# Get a vector of the Nt for each year
sequence_Nt <- calculate_Nt(K, N0, r, from_1950_to_2025)

# Put them in a matrix
pop_growth_matrix <- matrix(c(from_1950_to_2025, sequence_Nt), ncol=2)

# For fun: put them in a dataframe
pop_growth_df <- data.frame(from_1950_to_2025, sequence_Nt)


### Objective 2 ###
# We're asked to only do till year 2000
only_till_2000 <- seq(1950, 2000)

## Generic Plotting ##

# Base plot and first line
plot(x=only_till_2000, y=calculate_Nt(K, N0, r, only_till_2000), type="l", 
     col="blue", lwd=3, cex.lab=1.25,
     xlab="Year", ylab="Population Size", main="Population Growth Models")

# Second line
lines(x=only_till_2000, y=calculate_Nt(K, N0, r * 2, only_till_2000), 
      col="gold", lwd=3)

# Legend
legend(x="bottomright", fill=c("blue", "gold"), legend=c("r = 0.0005", "r = 0.0010"))


### Objective 3 ###

# Start the png saving process
png(filename="./Figures/base_pop_growth_models.png", height=480, width=640)

# Specify the plot to be saved
plot(x=only_till_2000, y=calculate_Nt(K, N0, r, only_till_2000), type="l", 
     col="blue", lwd=3, cex.lab=1.25, 
     xlab="Year", ylab="Population Size", main="Population Growth Models")
lines(x=only_till_2000, y=calculate_Nt(K, N0, r * 2, only_till_2000), 
      col="gold", lwd=3)
legend(x="bottomright", fill=c("blue", "gold"), legend=c("r = 0.0005", "r = 0.0010"))

# End the process
dev.off()
