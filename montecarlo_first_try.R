# plan for the power analysis of PSEs
# 
# pooled standard deviation = sqrt(SD_G1^2 + SD_G2^2 / 2)
# lets use existing data for now 

# preparing the data ------------------------------------------------------

library(quickpsy)
library(reshape)

# Load the RData file
load("C:/Users/Ben/Downloads/PSE_table.RData")

# Calculate mean difference for launch condition
launch_mean_diff <- mean(PSE_table[PSE_table$conditionlong == "launch_post_test",]$thre - 
                           PSE_table[PSE_table$conditionlong == "launch_pre",]$thre)

# Calculate mean difference for no context condition
no_context_mean_diff <- mean(PSE_table[PSE_table$conditionlong == "no_context_post_test",]$thre - 
                               PSE_table[PSE_table$conditionlong == "no_context_pre",]$thre)

# Calculate standard deviations for launch and no context conditions
l_diff_sd <- sd(PSE_table[PSE_table$conditionlong == "launch_post_test",]$thre - 
                  PSE_table[PSE_table$conditionlong == "launch_pre",]$thre)
nc_diff_sd <- sd(PSE_table[PSE_table$conditionlong == "no_context_post_test",]$thre - 
                   PSE_table[PSE_table$conditionlong == "no_context_pre",]$thre)

# Calculate pooled standard deviation
pooled_sd <- sqrt((l_diff_sd^2 + nc_diff_sd^2)/2)

# Calculate effect size
effect_size <- abs(launch_mean_diff - no_context_mean_diff) / pooled_sd

# simulation --------------------------------------------------------------

library(dplyr)
library(foreach)

set.seed(123)

for (n in 2:20) {
  
  sims = foreach(i = 1:10000, .combine = c) %do% {
    
    # Simulating data where the alternative hypothesis is true 
    placebo = rnorm(n, mean = launch_mean_diff, sd = l_diff_sd)
    treatment = rnorm(n, mean = no_context_mean_diff, sd = nc_diff_sd)
    
    # Run the hypothesis test with a 5% level
    test = t.test(placebo, treatment, conf.level = 0.95)
    
    # Check if null was rejected
    # aka is the value for the null hypothesis in the CI?
    result = (!between(0, test$conf.int[1], test$conf.int[2])) %>%
      as.integer()
  }
  
  # Calculate the sample average of the simulations 
  power = mean(sims)
  
  # Stop if we've achieved 80% power
  if (power > 0.8)
    break
}

# if we want truly random data, we need random parameters for pmf that change dependent on effect size but idk how