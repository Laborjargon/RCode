# cleaned up version 
# Define functions
discs = seq(0,1,length.out=7)
logistic_fun2 <- function(x, p) {(1 + exp(-(p[1]+p[2]*x)))^(-1)}
logistic_fun2_shift <- function(x, p, s) {(1 + exp(-(p[1]+(p[2]+s)*x)))^(-1)}

# Define simulated parameter values and create fake person data frame
simulated_parameter_val_1 <- rnorm(8, 5, 1)
simulated_parameter_val_2 <- rnorm(8, -10, 1)
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