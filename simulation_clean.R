# cleaned up version 
# Define functions
discs = seq(0,1,length.out=7)
logistic_fun2 <- function(x, p) {(1 + exp(-(p[1]+p[2]*x)))^(-1)}
logistic_fun2_shift <- function(x, p, s) {(1 + exp(-(p[1]+(p[2]+s)*x)))^(-1)}

# Define simulated parameter values and create fake person data frame
simulated_parameter_val_1 <- rnorm(8, 5, 0.25)
simulated_parameter_val_2 <- rnorm(8, -10, 0.25)
simulated_parameter_vals <- data.frame(simulated_parameter_val_1, simulated_parameter_val_2)

fake_person_list <- lapply(1:8, function(i) {
  person_name <- paste0("fake_person", i)
  c(person_name, simulated_parameter_val_1[i], simulated_parameter_val_2[i])
})
fake_person_df <- data.frame(do.call(rbind, fake_person_list))
colnames(fake_person_df) <- c("PersonNr", "Parameter_Val_1", "Parameter_Val_2")


# Simulate causal report data
n <- 20
for (i in 1:8) {
  # Get parameter values for current person
  param_vals <- as.numeric(c(fake_person_df$Parameter_Val_1[i], fake_person_df$Parameter_Val_2[i]))
  
  # Determine proportion of causal reports without shift
  answer_mean <- logistic_fun2(discs, param_vals)
  
  # Determine proportion of causal reports with shift
  shift_neg <- rnorm(1, -0.25, 0.05)
  answer_mean_shift_neg <- logistic_fun2_shift(discs, param_vals, shift_neg)
  shift_pos <- rnorm(1, 0.25, 0.05)
  answer_mean_shift_pos <- logistic_fun2_shift(discs, param_vals, shift_pos)
  
  # Simulate n trials for each disc overlap based on both means
  answers <- as.vector(mapply(rbinom, n, 1, answer_mean))
  answers_shift_neg <- as.vector(mapply(rbinom, n, 1, answer_mean_shift_neg))
  answers_shift_pos <- as.vector(mapply(rbinom, n, 1, answer_mean_shift_pos))
  
  # Combine in nice data frame
  person_data <- data.frame(discs = rep(discs, each = n),
                            answers = c(answers, answers_shift_neg, answers_shift_pos),
                            person = rep(i, each = n * 21),
                            shift = rep(c("no", "neg", "pos"), each = n * 7)) # each = n -> 20 per condition n*7 whole condition
  
  # Append to master data frame
  if (i == 1) {
    master_data <- person_data
  } else {
    master_data <- rbind(master_data, person_data)
  }
}

# grouping ----------------------------------------------------------------

master_data$person = as.factor(master_data$person)
master_data$shift = as.factor(master_data$shift)
pmf_master_data = quickpsy(master_data,discs,answers,guess=0,lapses=FALSE,prob=0.5,grouping=.c("shift", "person"),fun=logistic_fun2,parini=list(c(1,15),c(-15,-1)), bootstrap = "none")
xvals = seq(0,1,length.out=100)
# xvals_df <- data.frame(discs = rep(xvals, times = 8)) # number of people


# this is no longer clean code --------------------------------------------

for (p in levels(master_data$person)) {
  for (s in levels(master_data$shift)) {
    # Subset the data for the current person and condition
    data_subset <- subset(master_data, person == p & shift == s)
    
    # Compute the psychometric function using quickpsy
    pmf <- quickpsy(data_subset, discs, answers, guess = 0, lapses = FALSE, prob = 0.5, fun = logistic_fun2, parini = list(c(1, 15), c(-15, -1)),
                    bootstrap = "none")
    
    # Extract the PSE
    PSE <- pmf$thresholds[1]
    #print(paste(PSE, p))
    
    # Define x values for plotting
    xvals <- seq(0,1,length.out=100)
    
    # Compute predicted probabilities using the estimated parameters
    pred <- logistic_fun2(xvals, pmf$par$par)
    
    # Plot the psychometric function
    plot(xvals, pred, type = "l", xlab = "Disc overlap", ylab = "Proportion causal report")
    
    # Add a dashed line for the PSE
    abline(h = PSE, lty = 2)
    
    # Add a dashed line for the 50% probability threshold
    # abline(h = 0.5, lty = 2)
    
    # Add the means from the simulated data to the plot
    m2 <- melt(data_subset, id = c("discs"), measure = c("answers"))
    c2 <- cast(m2, discs ~ variable, mean)
    points(c2$discs, c2$answers)
    
    # Add a title for the plot
    title(paste("Psychometric function for Person", p, "and", s, "condition"))
  }
}
# make plot in ggplot because this is revolting to look at


# ANOVA within persona non grata ---------------------------------------------------------

library(tidyr)

# Convert the data to wide format
# wide_data <- pivot_wider(master_data, id_cols = person, names_from = shift, values_from = answers)
# this is interesting but useless

pse_data = data.frame(person = character(), shift = character(), PSE = numeric())
for (p in levels(master_data$person)) {
  for (s in levels(master_data$shift)) {
    # Subset the data for the current person and condition
    data_subset <- subset(master_data, person == p & shift == s)
    
    # Compute the psychometric function using quickpsy
    pmf <- quickpsy(data_subset, discs, answers, guess = 0, lapses = FALSE, prob = 0.5, fun = logistic_fun2, parini = list(c(1, 15), c(-15, -1)),
                    bootstrap = "none")
    
    # Extract the PSE
    PSE <- pmf$thresholds[1]
    
    # Add the PSE to the data frame
    pse_data <- rbind(pse_data, data.frame(person = p, shift = s, PSE = PSE))
  }
}

pse_wide <- pivot_wider(pse_data, id_cols = person, names_from = shift, values_from = thre)

#library(tidyverse)
#library(ggpubr)
library(rstatix)
# Convert the structure into long format again because why not 
pse_long <- pse_wide %>%
  gather(key = "condition", value = "PSE", neg, no, pos) %>%
  convert_as_factor(person, condition)

pse_long %>%
  group_by(condition) %>%
  get_summary_stats(PSE, type = "mean_sd")

pse_anova = anova_test(data = pse_long, dv = PSE, wid = person, within = condition)
get_anova_table(pse_anova)
# pse standarised?
# pairwise comparisons
pwc <- pse_long %>%
  pairwise_t_test(
    PSE ~ condition, paired = TRUE,
    p.adjust.method = "bonferroni"
  )
pwc
#bxp <- ggboxplot(pse_long, x = "condition", y = "PSE", add = "point")
#bxp
# pse ranks, packages not working is problem, p.adj. 1 is problem 