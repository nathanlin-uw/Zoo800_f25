# Quantitative Methods in Ecology and Evolution, Homework 12
# Nathan Lin
# November 18, 2025
# Generalized Linear Models

library(readxl)
library(lubridate)
library(dplyr)
library(ggplot2)

set.seed(1234)

### Objective 1A - plot probability density function
# Load in the data first
raw_bass <- read_xlsx("./HW 12/BSB_tagging_data.xlsx")

# Notes on the black sea bass data:
  # These fish change from female to male, some can start as male
  # Can think of intersex as having changed sex (F -> M)
  # Capture-recapture dates aren't too important to us
  # We are focusing on length at capture + DID FEMALES CHANGE SEX

# How come we saw some males at capture being recorded as intersex at recapture?
  # Is this just an incorrect assessment?

# Simplifying the data since we only need fish that started female
  # and only fish captured after July
our_focus_bass <- raw_bass %>% 
  filter(Sex_at_capture == "F", month(Date_at_recapture) > 7) 

# Getting our observed proportion
number_changed_successes <- our_focus_bass %>% filter(Sex_at_recapture != "F") %>% nrow()
number_total <- our_focus_bass %>% nrow()
number_failures <- number_total - number_changed_successes
observed_proportion <- number_changed_successes / number_total

# Remember beta distribution is for when we have the number of successes 
  # and the number of tries, and we are trying to find the probability of p
  # being different things.
proportions_zero_to_one <- seq(0, 1, by=0.05)
alpha <- number_changed_successes + 1
beta <- number_failures + 1
beta_densities <- dbeta(x=proportions_zero_to_one, shape1=alpha, shape2=beta)
df_for_beta_plot <- data.frame(proportions=proportions_zero_to_one, probability_density=beta_densities)
# Plotting the probability density function for proportions
ggplot(df_for_beta_plot, aes(x=proportions, y=probability_density)) + geom_line()

### Objective 1B - 95% CI for probability of sex change
# I will use qbeta to get the quantile values for the 2.5th and 97.5th quantiles
# For the 2.5th quantile
lower <- qbeta(0.025, shape1=alpha, shape2=beta)
upper <- qbeta(0.975, shape1=alpha, shape2=beta)
proportions_95_CI <- c(lower, upper)
proportions_95_CI
# 0.173 to 0.494

### Objective 2A - GLM for length and sex change
# We need to see if the length of the female influences its probability of sex change
# I will do 1 for change, 0 for no change
glm_bass_data <- our_focus_bass %>% 
  mutate(changed = ifelse(Sex_at_recapture != "F", 1, 0)) %>% 
  select(changed, Length_at_capture)
plot(changed ~ Length_at_capture, glm_bass_data)
# I'm doing approach with the 2-column matrix of successes and failures as the y 
bass_model <- glm(cbind(changed, 1 - changed) ~ Length_at_capture, family=binomial, data=glm_bass_data)
summary(bass_model)
# This gave me no significance with a p value for length of 0.112, so with this model
  # it does not look like length of a female influences its probability of sex change.

# However I am not sure that I am accurately representing the probability of sex change here.
# How can I get probability of sex change for a single individual?
# With beta distribution we had the p for the binomial but that's for everything
  # I suppose that's under the hood in the linear model

### Objective 2B - log-odds predicted change
# I hear we are supposed to do this as if length was significant

# Log(odds) = beta0 + beta1*x1 = log(p/(1-p)) 
# So log odds is just the prediction from the GLM
# Aka log odds = -14.377 + 0.045 * length

# So if we increase our length by 1 millimeter, 
  # we expect an increase of 0.045 in the log odds of sex change.

### Objective 2C - plotting sex change probability by length
bass_predictions <- predict.glm(object=bass_model, newdata=our_focus_bass, type="response")

glm_bass_data$predictions <- bass_predictions

# Plotting the relationship on the actual data
ggplot(glm_bass_data, aes(x=Length_at_capture, y=predictions)) + 
  geom_line() + 
  geom_point(aes(x=Length_at_capture, y=changed)) + 
  ylim(0, 1) + 
  labs(x="Length (mm)", 
       y="Probability of sex change",
       caption="This graph depicts the relationship between bass length and probability of sex change with the solid line. \nData points represent bass that did or did not change sex. As bass length increases, the probability of \nsex change increases.") + 
  theme_bw() + 
  theme(plot.caption=element_text(hjust=0))
