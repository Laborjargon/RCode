# -----------------------------------------------------------------------------
# Fitting a psychometric function

# by Sven Ohl
# 23.3.23
# -----------------------------------------------------------------------------

# -----------------------------------------------------------------------------
# load package
library(quickpsy)
library(reshape)
# -----------------------------------------------------------------------------

# -----------------------------------------------------------------------------
# simulate data
logistic_fun2 <- function (x, p) {(1 + exp(-(p[1]+p[2]*x)))^(-1)}

# The seven disc overlaps used in the experiment
discs <- seq(0,1,length.out=7)

# Set the parameters here for simulation. We actually want to estimate these
# parameters in the real experiment
param_vals <- c(5,-10)   

# determine proportion of causal reports
answer_mean <- logistic_fun2(discs,param_vals)

# simulate n trials for each disc overlap bases on answer_mean
# 0 = pass is reported
# 1 = launch is reported

n <- 20 #number of trials per disc overlap
answers <- as.vector(mapply(rbinom,n,1,answer_mean))

# combine in nice data frame
data <- data.frame(discs=rep(discs,each=n),answers=answers)
# -----------------------------------------------------------------------------

# -----------------------------------------------------------------------------
# fit psychometric function with two parameters
pmf <- quickpsy(data,discs,answers,guess=0,lapses=FALSE,prob=0.5,fun=logistic_fun2,parini=list(c(1,15),c(-15,-1)), bootstrap = "none")
# -----------------------------------------------------------------------------

# -----------------------------------------------------------------------------
# extract pse
PSE <- pmf$thresholds
# -----------------------------------------------------------------------------

# -----------------------------------------------------------------------------
# visualize psychometric function
xvals <- seq(0,1,length.out=100)
pred <- logistic_fun2(xvals,pmf$par$par)

plot(xvals,pred,type="l",xlab="Disc overlap", ylab="Proportion causal report")
abline(h=PSE[2],lty=2)
abline(v=PSE[1],lty=2)

# add means from simulated data to the plot
m2 <- melt(data,id=c("discs"),measure=c("answers"))
c2 <- cast(m2,discs ~ variable,mean)

points(c2$discs,c2$answers)
# -----------------------------------------------------------------------------

# -----------------------------------------------------------------------------
# Fragen zum Weiterdenken:
# Welche Werte f???r die Parameter hat die quickpsy function errechnet?

pmf$par$par[1]
pmf$par$par[2] # nicht gleich weil normalverteilung mit 20 wdh um parameter

# Sind das die gleichen Zahlen, die Du auch f???r die Simulation genutzt hast?
# theoretisch sogar ja weil man n -> inf dann sollten sie konvergieren
# Wenn ja, oder nein warum? 
# s. o. 
# Falls nein, welche Zeile im Script k???nntest Du anpassen um auf sehr ???hnliche Zahlen zu kommen?
# n = 1000 line 32
# -----------------------------------------------------------------------------

# -----------------------------------------------------------------------------
# Next steps:
# 1. Simulate a second condition with a leftward-shift
discs = seq(0,1,length.out=7)
param_vals = c(5,-10)
shift = -0.3 
n = 20 # n bleibt erstmal 20 

logistic_fun2_shift = function(x, p, shift) {
  (1 + exp(-(p[1] + p[2] * (x - shift))))^(-1) # "neue" funktion mit shift parameter: a + b(x-shift)
}
# shift parameter sch???tzen lassen?
answer_mean_shift = logistic_fun2_shift(discs, param_vals, shift)

answers_shift = as.vector(mapply(rbinom, n, 1, answer_mean_shift))

data_shift = data.frame(discs=rep(discs,each=n),answers=answers_shift)

# 2. Fit both psychometric functions by using qickpsy only once.
# combine the data from shift and original

data_comb = rbind(data, data_shift)
data_comb$condition = rep(c("original", "shifted"), each = 140) # 140 bad, better expression n * 7 oder so

# -> hint: Use the grouping in the quickpsy function
pmf_comb <- quickpsy(data_comb, discs, answers,
                     guess=0,
                     lapses=FALSE,
                     prob=0.5,
                     fun=logistic_fun2,
                     parini=list(c(1,15),c(-15,-1)),
                     bootstrap = "none") # doesnt work aber idk how to make it work grouping nicht drin ??

# 2.5 fitting both psychometric functions by using quickpsy only twice.
pmf_shift = quickpsy(data_shift, discs, answers, guess=0, 
                     lapses=FALSE,
                     prob=0.5,
                     fun=logistic_fun2,
                     parini=list(c(1,15),c(-15,-1)),
                     bootstrap = "none") # shift nicht in function, fehler ?

PSE_shift = pmf_shift$thresholds

pmf <- quickpsy(data,discs,answers,guess=0,lapses=FALSE,prob=0.5,fun=logistic_fun2,parini=list(c(1,15),c(-15,-1)), bootstrap = "none")
PSE = pmf$thresholds

# 3. Extract the PSEs in each condition and visualize (plot) them
# extractet sind sie 
xvals <- seq(0,1,length.out=100)
pred <- logistic_fun2(xvals,pmf$par$par)

plot(xvals,pred,type="l",xlab="Disc overlap", ylab="Proportion causal report")
abline(h=PSE[2],lty=2)
abline(v=PSE[1],lty=2)

# add means from simulated data to the plot
m2 <- melt(data,id=c("discs"),measure=c("answers"))
c2 <- cast(m2,discs ~ variable,mean)

points(c2$discs,c2$answers)

# shifted plot
xvals <- seq(0,1,length.out=100)
pred <- logistic_fun2(xvals,pmf_shift$par$par)

plot(xvals,pred,type="l",xlab="Disc overlap", ylab="Proportion causal report")
abline(h=PSE_shift[2],lty=2)
abline(v=PSE_shift[1],lty=2)

# add means from simulated data to the plot
m2 <- melt(data_shift,id=c("discs"),measure=c("answers"))
c2 <- cast(m2,discs ~ variable,mean)

points(c2$discs,c2$answers)
# 4. Visualize both psychometric functions next to each other.
par(mfrow = c(1, 2))

xvals <- seq(0,1,length.out=100)
pred <- logistic_fun2(xvals,pmf$par$par)

plot(xvals,pred,type="l",xlab="Disc overlap", ylab="Proportion causal report")
abline(h=PSE[2],lty=2)
abline(v=PSE[1],lty=2)


m2 <- melt(data,id=c("discs"),measure=c("answers"))
c2 <- cast(m2,discs ~ variable,mean)

points(c2$discs,c2$answers)

xvals <- seq(0,1,length.out=100)
pred <- logistic_fun2(xvals,pmf_shift$par$par)

plot(xvals,pred,type="l",xlab="Disc overlap", ylab="Proportion causal report")
abline(h=PSE_shift[2],lty=2)
abline(v=PSE_shift[1],lty=2)

# add means from simulated data to the plot
m2 <- melt(data_shift,id=c("discs"),measure=c("answers"))
c2 <- cast(m2,discs ~ variable,mean)

points(c2$discs,c2$answers)
# psychometric curves for sven's data-----------------------------------------------------------------------------
COC_file = read.table("C:/Users/somme/Documents/R/Data/Coc1011.dat")
# depending on working computer:
# COC_file = read.table("C:/Users/Ben/Desktop/RSHIT/Data/COC/Coc1011.dat")
# split data into four conditions 1: kein Context 2: Launch 4: Pass

coc_no_context = COC_file[COC_file$V9 == 1, ]
coc_launch = COC_file[COC_file$V9 == 2, ]
coc_pass = COC_file[COC_file$V9 == 4, ]

# drop columns except 6, 26 6: discs 29: bin_answer

launch_reduced = subset(coc_launch, select = c(6, 26))
no_context_reduced = subset(coc_no_context, select = c(6, 26))
pass_reduced = subset(coc_pass, select = c(6, 26))

# psychometric curves and PSE 
# V6 = discs & V26 = answers
# rename variables 
colnames(launch_reduced)[1] = "discs" 
colnames(launch_reduced)[2] = "answers" 

colnames(no_context_reduced)[1] ="discs"
colnames(no_context_reduced)[2] ="answers"

colnames(pass_reduced)[1] ="discs"
colnames(pass_reduced)[2] ="answers"     

# sortieren von discs 0.0 - 1
library('dplyr')

launch_sorted = arrange(launch_reduced, discs)

no_context_sorted = arrange(no_context_reduced, discs)

pass_sorted = arrange(pass_reduced, discs)

# psychometric fitting
pmf_launch = quickpsy(launch_sorted,discs,answers,guess=0,lapses=FALSE,prob=0.5,fun=logistic_fun2,parini=list(c(1,15),c(-15,-1)), bootstrap = "none")
launch_pse = pmf_launch$thresholds # 0.6363651

pmf_pass = quickpsy(pass_sorted,discs,answers,guess=0,lapses=FALSE,prob=0.5,fun=logistic_fun2,parini=list(c(1,15),c(-15,-1)), bootstrap = "none")
pass_pse = pmf_pass$thresholds # 0.46575

pmf_no_context = quickpsy(no_context_sorted,discs,answers,guess=0,lapses=FALSE,prob=0.5,fun=logistic_fun2,parini=list(c(1,15),c(-15,-1)), bootstrap = "none")
no_context_pse = pmf_no_context$thresholds # 0.5416502

# visualisation
# launch
par(mfrow = c(1, 3))
xvals <- seq(0,1,length.out=100)
pred_launch <- logistic_fun2(xvals,pmf_launch$par$par)

plot(xvals,pred_launch,type="l",xlab="Disc overlap Launch Context", ylab="Proportion causal report", main = paste0("PSE = ", round(launch_pse$thre, 3)))
abline(h=launch_pse[2],lty=2)
abline(v=launch_pse[1],lty=2)

# add means from simulated data to the plot
m2 <- melt(launch_sorted,id=c("discs"),measure=c("answers"))
c2 <- cast(m2,discs ~ variable,mean)

points(c2$discs,c2$answers)

# pass 
xvals <- seq(0,1,length.out=100)
pred_pass <- logistic_fun2(xvals,pmf_pass$par$par)

plot(xvals,pred_pass,type="l",xlab="Disc overlap Pass Context", ylab="Proportion causal report", main = paste0("PSE = ", round(pass_pse$thre, 3)))
abline(h=pass_pse[2],lty=2)
abline(v=pass_pse[1],lty=2)

# add means from simulated data to the plot
m2 <- melt(pass_sorted,id=c("discs"),measure=c("answers"))
c2 <- cast(m2,discs ~ variable,mean)

points(c2$discs,c2$answers)

# no context
xvals <- seq(0,1,length.out=100)
pred_no_context <- logistic_fun2(xvals,pmf_no_context$par$par)

plot(xvals,pred_no_context,type="l",xlab="Disc overlap No Context", ylab="Proportion causal report", main = paste0("PSE = ", round(no_context_pse$thre, 3)))
abline(h=no_context_pse[2],lty=2)
abline(v=no_context_pse[1],lty=2)

# add means from simulated data to the plot
m2 <- melt(no_context_sorted,id=c("discs"),measure=c("answers"))
c2 <- cast(m2,discs ~ variable,mean)

points(c2$discs,c2$answers)

# data in the same plot 
library(ggplot2)

tripleplot_data = data.frame(
  xvals = seq(0,1,length.out=100),
  launch_curve = pred_launch,
  pass_curve = pred_pass,
  no_context_curve = pred_no_context
)

triple_plot = ggplot(tripleplot_data, aes(x = xvals)) +
  geom_line(aes(y = pred_launch), color = "blue", size = 1.5) + 
  geom_line(aes(y = pred_pass), color = "red", size = 1.5) +
  geom_line(aes(y = pred_no_context), color = "green", size = 1.5) +
  labs(title = "Psychometric Curves",
       x = "Disc Overlap",
       y = "Proportion Causal Report")

PSE_launch = pmf_launch$thresholds$thre
PSE_pass = pmf_pass$thresholds$thre
PSE_no_context = pmf_no_context$thresholds$thre

triple_pse = data.frame(x = 0.5,
                        PSE = c(PSE_launch, PSE_pass, PSE_no_context), 
                        label = c("PSE Launch", "PSE Pass", "PSE No Context"))


triple_plot = triple_plot + scale_color_manual(values = c("PSE Launch" = "blue", "PSE Pass" = "red", "PSE No Context" = "green")) 

triple_plot = triple_plot + geom_point(data = triple_pse, aes(x = PSE, y = x, color = label), size = 3, shape = 21, fill = "white")

print(triple_plot)

# grouping ----------------------------------------------------------------


launch_sorted$condition = 1 # 1 = launch
pass_sorted$condition = 2 # 2 = pass
no_context_sorted$condition = 3 # 3 = no_context 
grouped_data = rbind(launch_sorted, pass_sorted, no_context_sorted)
grouped_data$condition_fac<-as.factor(grouped_data$condition)

quickpsy(grouped_data,discs,answers,guess=0,lapses=FALSE,prob=0.5,grouping=.c("condition_fac"),fun=logistic_fun2,parini=list(c(1,15),c(-15,-1)), bootstrap = "none")

# n personen generieren und iterieren -------------------------------------
# lets say we want to generate data for 8 people
# -> we need to draw random data around the parameters 8 times
# whats the standard deviation of the normal distribution from which we choose 
simulated_parameter_val_1 = rnorm(8, 5, 1) # value is 5
simulated_parameter_val_2 = rnorm(8, -10, 1) # value = -10

simulated_parameter_vals = data.frame(simulated_parameter_val_1, simulated_parameter_val_2)

fake_person1 = c(simulated_parameter_vals$simulated_parameter_val_1[1], simulated_parameter_vals$simulated_parameter_val_2[1]) 
# normalverteilung um paramateer f???r alle personen
person_list = list()

for (i in 1:8) {
  person_name = paste0("fake_person", i)
  person_list[[i]] = c(person_name, simulated_parameter_val_1[i], simulated_parameter_val_2[i])
}
fake_person_df = data.frame(do.call(rbind, person_list))
colnames(fake_person_df) <- c("PersonNr", "Parameter_Val_1", "Parameter_Val_2")
# below is from chatgpt
fake_person_df$param_vals = paste0("(", fake_person_df$Parameter_Val_1, ", ", fake_person_df$Parameter_Val_2, ")")

# for loop f???r jeweiligen personen und generate datensatz 
# VPN erkennbar machen UND condition
simulation = data.frame(discs = "", # disc overlap
                        answers = "", # 0:1 
                        condition_fac = "", # condition 1:3 as factor 
                        pid = "") # 1:8 
simulation$condition_fac = as.factor(simulation$condition_fac)

# n * 420 observations 

# simulating normal parameters --------------------------------------------

# for loop for simulating -------------------------------------------------

for (i in 1:8) {
  # Get parameter values for current person
  param_vals <- as.numeric(c(fake_person_df$Parameter_Val_1[i], fake_person_df$Parameter_Val_2[i]))
  
  # Determine proportion of causal reports
  answer_mean <- logistic_fun2(discs, param_vals)
  
  # Simulate n trials for each disc overlap based on answer_mean
  answers <- as.vector(mapply(rbinom, n, 1, answer_mean))
  
  # Combine in nice data frame
  person_data <- data.frame(discs=rep(discs, each=n), answers=answers, pid=i)
  
  # Append to master data frame
  if (i == 1) {
    master_data <- person_data
  } else {
    master_data <- rbind(master_data, person_data)
  }
}

# shifted data and normal data --------------------------------------------
# logistic_fun2 = function (x, p) {(1 + exp(-(p[1]+p[2]*x)))^(-1)}
# n = 20
for (i in 1:8) {
  # Get parameter values for current person
  param_vals <- as.numeric(c(fake_person_df$Parameter_Val_1[i], fake_person_df$Parameter_Val_2[i]))
  
  answer_mean <- logistic_fun2(discs, param_vals)
  
  # with shift
  shift <- rnorm(1, -0.25, 0.05)
  answer_mean_shift <- logistic_fun2_shift(discs, param_vals, shift)
  
  # Simulate n trials for each disc overlap based on both
  #answers <- as.vector(rbinom(n * 7, 1, answer_mean))
  answers = as.vector(mapply(rbinom, n, 1, answer_mean))
  #answers_shift <- as.vector(rbinom(n * 7, 1, answer_mean_shift))
  answers_shift = as.vector(mapply(rbinom, n, 1, answer_mean_shift))
  # Combine in nice data frame
  person_data <- data.frame(discs = rep(discs, each = n),
                            answers = c(answers, answers_shift),
                            person = rep(i, each = n * 14),
                            shift = c(rep("no", each = n * 7), rep("yes", each = n * 7)))
  
  # Append to master data frame
  if (i == 1) {
    master_data <- person_data
  } else {
    master_data <- rbind(master_data, person_data)
  }
}

# including all 3 conditions ----------------------------------------------
# how shift: shift = rep(shift_factor, each = n * 7) in df 
n = 20
for (i in 1:8) {
  # Get parameter values for current person
  param_vals <- as.numeric(c(fake_person_df$Parameter_Val_1[i], fake_person_df$Parameter_Val_2[i]))
  
  # without shift
  answer_mean <- logistic_fun2(discs, param_vals)
  
  # with shift
  shift_neg <- rnorm(1, -0.25, 0.05)
  answer_mean_shift_neg <- logistic_fun2_shift(discs, param_vals, shift_neg)
  
  # positive shift 
  shift_pos <- rnorm(1, 0.25, 0.05)
  answer_mean_shift_pos <- logistic_fun2_shift(discs, param_vals, shift_pos)
  
  # Simulate n trials for each disc overlap based on condition
  answers <- as.vector(mapply(rbinom, n, 1, answer_mean))
  answers_shift_neg <- as.vector(mapply(rbinom, n, 1, answer_mean_shift_neg))
  answers_shift_pos <- as.vector(mapply(rbinom, n, 1, answer_mean_shift_pos))
  
  # Combine in nice data frame
  person_data <- data.frame(discs = rep(discs, each = n),
                            answers = c(answers, answers_shift_neg, answers_shift_pos),
                            person = rep(i, each = n * 21),
                            shift = rep(c("no", "negative", "positive"), each = n * 7))
  
  # Append to master data frame
  if (i == 1) {
    master_data <- person_data
  } else {
    master_data <- rbind(master_data, person_data)
  }
}
master_data$person = as.factor(master_data$person)
master_data$shift = as.factor(master_data$shift)
pmf_master_data = quickpsy(master_data,discs,answers,guess=0,lapses=FALSE,prob=0.5,grouping=.c("shift", "person"),fun=logistic_fun2,parini=list(c(1,15),c(-15,-1)), bootstrap = "none")

pmf_master_data$par$par # 9.8 -9.3 -> NA why ?? 

# outsource script 
# plotten
library(ggplot2)

ggplot(master_data, aes(x = x, y = y, color = group1)) +
  geom_line() +
  facet_grid(. ~ group2)
# anova 
# post hoc ttest 3x 
