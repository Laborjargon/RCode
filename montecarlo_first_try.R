# plan for the power analysis of PSEs
# 
# placebo would be the table of PSEs for dPSE_L and treatment would be table of dPSE_NC
# pooled standard deviation = sqrt(SD_G1^2 + SD_G2^2 / 2)
# lets use existing data for now 

library(quickpsy)
library(reshape)


load("C:/Users/Ben/Downloads/PSE_table.RData")

launch_mean_diff <- mean(PSE_table[PSE_table$conditionlong == "launch_post_test",]$thre - 
                           PSE_table[PSE_table$conditionlong == "launch_pre",]$thre)
no_context_mean_diff <- mean(PSE_table[PSE_table$conditionlong == "no_context_post_test",]$thre - 
                               PSE_table[PSE_table$conditionlong == "no_context_pre",]$thre)
l_diff_sd <- sd(PSE_table[PSE_table$conditionlong == "launch_post_test",]$thre - 
                  PSE_table[PSE_table$conditionlong == "launch_pre",]$thre)
nc_diff_sd <- sd(PSE_table[PSE_table$conditionlong == "no_context_post_test",]$thre - 
                   PSE_table[PSE_table$conditionlong == "no_context_pre",]$thre)

pooled_sd <- sqrt((l_diff_sd^2 + nc_diff_sd^2)/2)
effect_size <- abs(launch_mean_diff - no_context_mean_diff) / pooled_sd


library(dplyr)
library(foreach)

for (n in 2:20) {
  
  sims = foreach(i = 1:10000, .combine = c) %do% {
    
    # Simulating data where the alternative hypothesis is true 
    # and the true difference is 0.5
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
  
  # Stop if we've acheived 80% power
  if (power > 0.8)
    break
}

