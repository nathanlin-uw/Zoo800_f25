# Quantitative Methods in Ecology and Evolution, Final Project
# Nathan Lin
# December 9, 2025
# Power analysis: differential expression analysis between two groups

library(dplyr)
library(ggplot2)
library(PROPER)
library(edgeR)

set.seed(1234)

##### My first attempt at simulating RNAseq count data #####
alpha <- 1
base_expression_levels_mu <- rgamma(n=7000, shape=alpha, scale=5)
# Group 1's expression levels are just our original ones
group1_read_counts <- rnbinom(n=7000, mu=base_expression_levels_mu, size=alpha)
# Take a look -- this code was used for Figures 1, 2, and 3
hist(group1_read_counts)

##### Simulating and detecting differential expression #####
simulate_count_data <- function(samples_per_group, effect_size, underlying_alpha=5, desired_mean) {
  # Gives a nice dataframe with samples as columns and individual genes as rows
  # Sample size should be 1 to 20 (must be int), effect sizes should be -3 to 3, alpha will be 5 (Yoon and Nam 2017), desired_mean will be 10 or 50 or 100
  # This is for one effect size (no splitting up our data to do multiple at once)
  # Constant things: 1000 genes (half of which are DE), gene length sequencing depth etc
  # Use paste0 for no space in between
  counts <- data.frame(row.names=paste0("g_", 1:1000))
  base_expression_levels_mu <- rgamma(n=1000, shape=underlying_alpha, scale=desired_mean/underlying_alpha)
  group2_expression_levels_mu <- c(base_expression_levels_mu[1:500], base_expression_levels_mu[501:1000] * (2 ^ effect_size))
  
  # For sanity check plot later
  par(mfrow=c(2, samples_per_group))
  
  # Loop for group 1
  for (sample_index in seq(samples_per_group)) {
    g1_sample_name <- paste0("group1_s", sample_index)
    # Group 1's expression levels are just our original ones
    counts[1:1000, g1_sample_name] <- rnbinom(n=1000, mu=base_expression_levels_mu, size=underlying_alpha)
    # Sanity check plot
    hist(counts[[g1_sample_name]], main=g1_sample_name, xlab="Read Counts")
  }
  # Loop for group 2
  for (sample_index in seq(samples_per_group)) {
    g2_sample_name <- paste0("group2_s", sample_index)
    counts[1:1000, g2_sample_name] <- rnbinom(n=1000, mu=group2_expression_levels_mu, size=underlying_alpha)
    # Sanity check plot
    hist(counts[[g2_sample_name]], main=g2_sample_name, xlab="Read Counts")
  }
  # Reset plot layout to default
  par(mfrow=c(1, 1))
  # Return the dataframe
  return(counts)
}

# Try it out
# View(simulate_count_data(3, 3, 1, 1))

simulated_count_data <- simulate_count_data(samples_per_group = 2, 
                                            effect_size = 2,
                                            underlying_alpha = 5,
                                            desired_mean = 3)

# This didn't work too well since I don't have good empirical information on the parameters and way to design the data (it'd take a bit too long to get those from data pulled from somewhere else)

##### My second attempt at data simulation using the PROPER package #####
# This uses the package "PROPER", which was built for RNAseq power analyses and whatnot 
# I am only going to use it for generating the data though
proper_simulation <- function(samples_per_group, effect_size) {
  # "Cheung" dataset is for unrelated individuals with large overdispersion 
  # Vs moderate dispersion from human liver comparisons (same species same tissue type), small dispersion from inbred mice (genetically similar), and very small dispersion from technical replicates (no biological variation)
  simulation_options <- RNAseq.SimOptions.2grp(ngenes=1000, lBaselineExpr="cheung", lOD="cheung", p.DE=0.5, lfc=effect_size, sim.seed=1234)
  simulation_output <- simRNAseq(simOptions=simulation_options, n1=samples_per_group, n2=samples_per_group)
  # Pull out the simulated data and IDs of differentially expressed genes
  differentially_expressed_IDs <- paste0("gene_", simulation_output$DEid)
  simulated_counts <- as.data.frame(simulation_output$counts, row.names=paste0("gene_", seq(1, 1000))) 
  # Return both of these
  return(list(simulated_counts, differentially_expressed_IDs))
}

test_sim_output <- proper_simulation(samples_per_group=3, effect_size=2)
test_sim_output[[1]]

par(mfrow=c(2,3))
hist(test_sim_output[[1]]$V1, main="Group 1 Sample 1", xlab="Read Counts")
hist(test_sim_output[[1]]$V2, main="Group 1 Sample 2", xlab="Read Counts")
hist(test_sim_output[[1]]$V3, main="Group 1 Sample 3", xlab="Read Counts")
hist(test_sim_output[[1]]$V4, main="Group 2 Sample 1", xlab="Read Counts")
hist(test_sim_output[[1]]$V5, main="Group 2 Sample 2", xlab="Read Counts")
hist(test_sim_output[[1]]$V6, main="Group 2 Sample 3", xlab="Read Counts")
par(mfrow=c(1,1))

##### Differential expression analysis #####
false_discovery_true_pos_rates <- function(count_data, DE_indices, samples_per_group) {
  # According to the edgeR documentation, I can use Fisher's exact test for single-factor experiments (that's us!)
  # Especially smaller data with common dispersion
  # I need to first make a DGE object
  # Filter it first by the expression levels (if expression of both is 0 we don't keep)
  
  indices_to_keep <- filterByExpr(count_data, group=c(replicate(samples_per_group, 1), replicate(samples_per_group, 2)))
  actual_gene_names_to_keep <- names(indices_to_keep[indices_to_keep])
  filtered_count_data <- count_data[indices_to_keep, ]
  remaining_DE_indices <- intersect(actual_gene_names_to_keep, DE_indices)
    
  edgeR_de_object <- DGEList(counts=filtered_count_data, group=c(replicate(samples_per_group, 1), replicate(samples_per_group, 2)))
  # They want to estimate common and tagwise dispersion
  dispersion_estimated <- estimateDisp(edgeR_de_object)
  # Do Fisher's exact test
  exact_test_results <- exactTest(dispersion_estimated)
  # Summary of the results
  topTags(exact_test_results)
  # Total number of significant results
  total_significant_count <- nrow(exact_test_results$table %>% filter(PValue < (0.05)))
  # Let's see true positives (genes that were truly differentially expressed)
  true_positives <- exact_test_results$table[remaining_DE_indices, ] %>% filter(PValue < 0.05)
  true_pos_rate <- nrow(true_positives) / nrow(filtered_count_data[remaining_DE_indices, ])
  # Let's see false positives (genes that weren't originally differentially expressed)
  false_positives <- total_significant_count - nrow(true_positives)
  # False discovery rate is the # of false positives / # of significant results
  false_discovery_rate <- false_positives / total_significant_count
  # Power = 1 - P(incorrectly keeping a false null) = freq(true positives)
  return(c(false_discovery_rate, true_pos_rate))
}

# false_discovery_true_pos_rates(count_data=test_sim_output[[1]], DE_indices=test_sim_output[[2]], samples_per_group=3)


power_false_pos_simulation <- function() {
  # Samples to try: 2-20 (we can't use the exact test for sample size of 1)
  # Effect sizes to try: -3, -2, -1, 0, 1, 2, 3
  sample_sizes <- seq(2, 20)
  effect_sizes <- seq(-3, 3)
  
  # Make a new dataframe for the power and FDR data (sample size, effect size, fdr, power)
  col_names <- c("sample_size", "effect_size", "false_discovery_rate", "true_positives")
  sim_accuracy_results <- as.data.frame(matrix(ncol=4, nrow=0, dimnames=list(NULL, col_names)))
  
  # Run the loop
  for (group_sample_size in sample_sizes) {
    for (effect_sizes_index in seq(length(effect_sizes))) {
      sim_output <- proper_simulation(samples_per_group=group_sample_size, effect_size=effect_sizes[effect_sizes_index])
      sim_analysis <- false_discovery_true_pos_rates(count_data=sim_output[[1]], DE_indices=sim_output[[2]], samples_per_group=group_sample_size)
      final_sim_results <- c(sample_size = group_sample_size, 
                             effect_size = effect_sizes[effect_sizes_index],
                             false_discovery_rate = sim_analysis[1],
                             true_positives = sim_analysis[2])
      # Add the simulation results to one row of the df
      sim_accuracy_results[nrow(sim_accuracy_results) + 1, ] <- final_sim_results
    }
  }
  return(sim_accuracy_results)
}

final_power_fdr_data <- power_false_pos_simulation()
# Need effect size as a factor
final_power_fdr_data$effect_size <- factor(final_power_fdr_data$effect_size)

# Plot false discovery rate (false positives / total significant)
fdr_plot <- final_power_fdr_data %>% 
  ggplot(aes(x=sample_size, y=false_discovery_rate, col=effect_size)) +
  geom_line(linewidth=1) + 
  scale_colour_brewer(palette="Set1") + 
  theme_bw()
# See the plot
fdr_plot

# Plot statistical power (true positives / total true)
power_plot <- final_power_fdr_data %>% 
  ggplot(aes(x=sample_size, y=true_positives, col=effect_size)) + 
  geom_line(linewidth=1) + 
  scale_colour_brewer(palette="Set1") + 
  theme_bw()
# See the plot
power_plot


#### Same as above, but just for GLM ####

glm_try <- function(count_data, DE_indices, samples_per_group) {
  # According to the edgeR documentation, I can use Fisher's exact test for single-factor experiments (that's us!)
  # Especially smaller data with common dispersion
  # I need to first make a DGE object
  indices_to_keep <- filterByExpr(count_data, group=c(replicate(samples_per_group, 1), replicate(samples_per_group, 2)))
  actual_gene_names_to_keep <- names(indices_to_keep[indices_to_keep])
  filtered_count_data <- count_data[indices_to_keep, ]
  remaining_DE_indices <- intersect(actual_gene_names_to_keep, DE_indices)
  edgeR_de_object <- DGEList(counts=filtered_count_data, group=c(replicate(samples_per_group, 1), replicate(samples_per_group, 2)))
  # They want to estimate common and tagwise dispersion
  dispersion_estimated <- estimateDisp(edgeR_de_object)
  # Fit GLM
  glm_fit <- glmFit(dispersion_estimated)
  # Ratio test for DE
  lrt.2vs1 <- glmLRT(glm_fit, coef=2)
  # Summary of the results
  topTags(lrt.2vs1)
  # Total number of significant results
  total_significant_count <- nrow(lrt.2vs1$table %>% filter(PValue < 0.05))
  
  # Let's see true positives (genes that were truly differentially expressed)
  true_positives <- lrt.2vs1$table[remaining_DE_indices, ] %>% filter(PValue < 0.05)
  true_pos_rate <- nrow(true_positives) / nrow(filtered_count_data[remaining_DE_indices, ])
  
  # Let's see false positives (genes that weren't originally differentially expressed)
  false_positives <- total_significant_count - nrow(true_positives)
  # False discovery rate is the # of false positives / # of significant results
  false_discovery_rate <- false_positives / total_significant_count
  
  print(paste("Total:", total_significant_count, 
              "... False:", false_positives, 
              "... True:", nrow(true_positives)))
  
  # Power = 1 - P(incorrectly keeping a false null) = freq(true positives)
  return(c(false_discovery_rate, true_pos_rate))
}

glm_try(test_sim_output[[1]], test_sim_output[[2]], samples_per_group=3)

power_false_pos_simulation_glm <- function() {
  # Samples to try: 2-20 (we can't use the exact test for sample size of 1)
  # Effect sizes to try: -3, -2, -1, 0, 1, 2, 3
  sample_sizes <- seq(2, 20)
  effect_sizes <- seq(-3, 3)
  
  # Make a new dataframe for the power and FDR data (sample size, effect size, fdr, power)
  col_names <- c("sample_size", "effect_size", "false_discovery_rate", "true_positives")
  sim_accuracy_results <- as.data.frame(matrix(ncol=4, nrow=0, dimnames=list(NULL, col_names)))
  
  # Run the loop
  for (group_sample_size in sample_sizes) {
    for (effect_sizes_index in seq(length(effect_sizes))) {
      sim_output <- proper_simulation(samples_per_group=group_sample_size, effect_size=effect_sizes[effect_sizes_index])
      sim_analysis <- glm_try(count_data=sim_output[[1]], DE_indices=sim_output[[2]], samples_per_group=group_sample_size)
      final_sim_results <- c(sample_size = group_sample_size, 
                             effect_size = effect_sizes[effect_sizes_index],
                             false_discovery_rate = sim_analysis[1],
                             true_positives = sim_analysis[2])
      # Add the simulation results to one row of the df
      sim_accuracy_results[nrow(sim_accuracy_results) + 1, ] <- final_sim_results
    }
  }
  return(sim_accuracy_results)
}

final_power_fdr_data_glm <- power_false_pos_simulation_glm()
# Need effect size as a factor
final_power_fdr_data_glm$effect_size <- factor(final_power_fdr_data_glm$effect_size)

# Plot false discovery rate (false positives / total significant)
fdr_plot_glm <- final_power_fdr_data_glm %>% 
  ggplot(aes(x=sample_size, y=false_discovery_rate, col=effect_size)) +
  geom_line(linewidth=1) + 
  scale_colour_brewer(palette="Set1") + 
  theme_bw()
# See the plot
fdr_plot_glm

# Plot statistical power (true positives / total true)
power_plot_glm <- final_power_fdr_data_glm %>% 
  ggplot(aes(x=sample_size, y=true_positives, col=effect_size)) + 
  geom_line(linewidth=1) + 
  scale_colour_brewer(palette="Dark2") + 
  theme_bw()
# See the plot
power_plot_glm



##### Most recent developments, beyond here I did not have time to finish #####

# Looking at actual data
birds_normalized <- data.frame(read.csv("./Final Project/birds_babesia.csv"))
one_sample <- birds_normalized[, 2]
# This is the right-skewed distribution that is standard for RNAseq data for one biological sample
hist(one_sample)

# What sorts of distribution would be good for representing this across-genes variation?
distribution_model_poisson <- fitdist(one_sample, "pois")
summary(distribution_model_poisson)   # AIC: 6.7e+13
distribution_model_neg_binom <- fitdist(one_sample, "nbinom")
summary(distribution_model_neg_binom)   # AIC: 6.7e+13
distribution_model_gamma <- fitdist(one_sample, "gamma")
summary(distribution_model_gamma)   # AIC: 74244
distribution_model_norm <- fitdist(one_sample, "norm")
summary(distribution_model_norm)   # AIC: 76432
distribution_model_lnorm <- fitdist(one_sample, "lnorm")
summary(distribution_model_lnorm)   # AIC: 73777
distribution_model_logis <- fitdist(one_sample, "logis")
summary(distribution_model_logis)   # AIC: 77466
distribution_model_exp <- fitdist(one_sample, "exp")
summary(distribution_model_exp)   # AIC: 93586

### What parameters best fit the negative binomial represented by the birds data? ###
library(parallel)

# Set up a list to hold vectors of length 3
list_of_threes <- list()

# For each gene, take the positive and the control data and put those into the list as separate entries
for (row_index in seq(nrow(birds_normalized))) {
  positive_samples <- as.numeric(birds_normalized[row_index, 2:4])
  control_samples <- as.numeric(birds_normalized[row_index, 5:7])
  list_of_threes <- append(list_of_threes, list(positive_samples, control_samples))
  print(paste("Finished row", row_index, "of", nrow(birds_normalized)))
}

# Takes a vector of three numbers and sees what nbinom distribution parameters would fit it
extract_parameter_estimates <- function(set_of_three, parameter_name) {
  # Fit distribution model
  dist_nbinom <- fitdist(set_of_three, "nbinom")
  # Pull out either the "mu" or "size" parameter estimate
  parameter_estimate <- dist_nbinom$estimate[parameter_name]
  # print("You're not going crazy waiting - things are actually happening.")
  return(parameter_estimate)
}

# This stuff is from homework 5, when we did bootstrap parallel computing
n_cores <- max(1, detectCores() - 1)        # use all but one core (be nice to your laptop)
cl <- makeCluster(n_cores)                  # start worker processes
clusterSetRNGStream(cl, iseed = 123)        # make random numbers reproducible across workers
# Send needed objects to workers (data + functions vector)
clusterExport(cl, varlist = c("list_of_threes", "extract_parameter_estimates", "fitdist"), envir = environment())

# Compute parameter estimates for each set of positive/control data for each gene
mu_estimates_parallel <- parLapply(cl, list_of_threes, extract_parameter_estimates, parameter_name="mu")
size_estimates_parallel <- parLapply(cl, list_of_threes, extract_parameter_estimates, parameter_name="size")
# Compute the average 
mu_estimate_average <- mean(as.numeric(mu_estimates_parallel)) # 7.43
mu_estimate_median <- median(as.numeric(mu_estimates_parallel)) # 6.80
size_estimate_average <- mean(as.numeric(size_estimates_parallel)) # 99.98
size_estimate_median <- median(as.numeric(size_estimates_parallel)) # 100

# So we are going to use a nbinom distribution for the biological replicates with size 100 and mu 7.43

# For the gene expression levels though we are going to use log normal
