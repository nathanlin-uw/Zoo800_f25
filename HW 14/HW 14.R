# Quantitative Methods in Ecology and Evolution, Homework 14
# Nathan Lin
# December 2, 2025
# Model selection and AIC

### Objective 1A - setting up the scenario ###

# Nurul's ecological scenario:
# We are investigating the impact of Chronic Wasting Disease (CWD) on white-tailed deer. 
# We used GPS collars with accelerometers to measure the average daily activity level (odba) of 100 deer. 
# We also know the age of each deer and its CWD test status (positive or negative).

# Does CWD status affect the daily activity level (odba) of deer? 
# After accounting for the natural decline in activity due to age, do CWD-positive deer have different activity levels than CWD-negative deer? 
# Furthermore, does the relationship between age and activity differ between CWD-positive and negative animals (i.e., do CWD-positive animals become lethargic more rapidly with age)?

# Load in data
cwd_data <- read.csv("./cwd_ancova_data_to_share.csv")

### Objective 1B - fit alternative models ###
cwd_full_model <- lm(odba ~ age_yrs + cwd_status + age_yrs:cwd_status, data=cwd_data)
cwd_no_interaction_model <- lm(odba ~ age_yrs + cwd_status, data=cwd_data)

### Objective 1C - logLik ###
# For full model we have NLL = 189.4108 (df=5)
logLik(cwd_full_model) # -189.4108
# For reduced model we have NLL = 195.5691 (df=4)
logLik(cwd_no_interaction_model) # -195.5691

# The full model has a lower negative log likelihood

### Objective 1D - likelihood ratio test ###
# Use lmtest::lrtest()
library(lmtest)
# Run lrt
lrtest(cwd_full_model, cwd_no_interaction_model)
# We get a significance of < 0.001, so the NLL is sufficiently lower to justify this extra parameter,
  # and the more complex model is preferred. 
# This suggests that CWD status does affect the relationship between age and activity.
  # We got the same thing with backward model selection.

### Objective 2A - AIC table ###
# Get the names for the models
model_names <- c("Full model", "Main effects (no int.)", "Only age", "Only CWD status", "Only intercept")

# We have the first two above already (cwd_full_model, cwd_no_interaction_model)
cwd_only_age_model <- lm(odba ~ age_yrs, data=cwd_data)
cwd_only_cwd_status_model <- lm(odba ~ cwd_status, data=cwd_data)
cwd_only_intercept_model <- lm(odba ~ 1, data=cwd_data)

# Calculate AICs
aic_full <- 2 * (-logLik(cwd_full_model) - 3)
aic_no_int <- 2 * (-logLik(cwd_no_interaction_model) - 2)
aic_age_only <- 2 * (-logLik(cwd_only_age_model) - 2)
aic_cwd_only <- 2 * (-logLik(cwd_only_cwd_status_model) - 2)
aic_intercept_only <- 2 * (-logLik(cwd_only_intercept_model) - 2)

# Compile AICs
model_aics <- c(aic_full, aic_no_int, aic_age_only, aic_cwd_only, aic_intercept_only)

# Make the table
aic_table <- data.frame(models=model_names, AIC=model_aics)
# View the table
aic_table


### Objective 2B - conclusion ###
# Only the full model is supported by AIC (everything else has a delta AIC of > 10).
# This supports the idea that the interaction between CWD status and age is important 
  # in modulating their individual effects on deer activity.