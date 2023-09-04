# multiperson analysis of data for pmf COCA 
########################################
# Bachelor thesis CoCa Analysis Script #
########################################
# data in the same plot 
library(ggplot2)
library(dplyr)
library(quickpsy)
library(reshape)
library(tidyr)
library(viridis)
library(rstatix)
library(stats)

testdir = list.files("C:/Users/somme/Desktop/Bachelorarbeit/Data/CoCa/Test")
combined_data = data.frame()

for (file in testdir) {
  data = read.table(paste0("C:/Users/somme/Desktop/Bachelorarbeit/Data/CoCa/Test/", file))
  # Check if the number of observations is greater than 672
  if (nrow(data) > 672) {
    data <- data[(nrow(data) - 671):nrow(data), ]
    
  }
  combined_data <- rbind(combined_data, data)
}

merged_data = combined_data

#source("C:/Users/somme/Desktop/Bachelorarbeit/Scripts/merging people.R")

logistic_fun2 <- function (x, p) {(1 + exp(-(p[1]+p[2]*x)))^(-1)}
discs <- seq(0,1,length.out=7)

#COC_file = read.table("C:/Users/somme/Desktop/Bachelorarbeit/Data/CoCa/Coc1082.dat") # dont use ...1 sessions dingus 
COC_file = merged_data
# depending on working computer:
# COC_file = read.table("C:/Users/Ben/Desktop/RSHIT/Data/COC/Coc1092.dat")
# split data into four conditions 1 no_context 2 launch 4 pass 

coc_no_context = COC_file[COC_file$V9 == 1, ]
coc_launch = COC_file[COC_file$V9 == 2, ]
coc_pass = COC_file[COC_file$V9 == 4, ]

# drop columns except 6, 26 6: discs 26: bin_answer

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

xvals <- seq(0,1,length.out=100)

pred_launch <- logistic_fun2(xvals,pmf_launch$par$par)
pred_pass <- logistic_fun2(xvals,pmf_pass$par$par)
pred_no_context <- logistic_fun2(xvals,pmf_no_context$par$par)


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
  labs(title = "Psychometric Curves Total",
       x = "Disc Overlap",
       y = "Proportion Causal Report") +
  xlim(0,1) + 
  ylim(0,1)

PSE_launch = pmf_launch$thresholds$thre
PSE_pass = pmf_pass$thresholds$thre
PSE_no_context = pmf_no_context$thresholds$thre

triple_pse = data.frame(x = 0.5,
                        PSE = c(PSE_launch, PSE_pass, PSE_no_context), 
                        curve_type = c("launch_curve", "pass_curve", "no_context_curve"))


triple_plot = triple_plot + scale_color_manual(values = c("launch_curve" = "blue", "pass_curve" = "red", "no_context_curve" = "green")) 

triple_plot = triple_plot + geom_point(data = triple_pse, aes(x = PSE, y = x, color = curve_type), size = 3, shape = 21, fill = "white")

print(triple_plot)


# adaptation conditions ---------------------------------------------------
# from original file, extract relevant columns

coc_reduced = subset(COC_file, select = c(3,6,9,26))

# and split data into pre-post adaptation

coc_pre_adapt = coc_reduced[coc_reduced$V3 == 0, ]
coc_pre_adapt = subset(coc_pre_adapt, select = -c(V3))

coc_post_adapt = coc_reduced[coc_reduced$V3 == 1, ]
coc_post_adapt = subset(coc_post_adapt, select = -c(V3))

# pre adaptation conditions 
pre_adapt_no_context = coc_pre_adapt[coc_pre_adapt$V9 == 1, ]
pre_adapt_launch = coc_pre_adapt[coc_pre_adapt$V9 == 2, ]
pre_adapt_pass = coc_pre_adapt[coc_pre_adapt$V9 == 4, ]

# post adaptation conditions 
post_adapt_no_context = coc_post_adapt[coc_post_adapt$V9 == 1,]
post_adapt_launch = coc_post_adapt[coc_post_adapt$V9 == 2,]
post_adapt_pass = coc_post_adapt[coc_post_adapt$V9 == 4,]

# reduce V9 six times?
# pre
pre_adapt_no_context = subset(pre_adapt_no_context, select= -c(V9))
pre_adapt_launch = subset(pre_adapt_launch, select= -c(V9))
pre_adapt_pass = subset(pre_adapt_pass, select= -c(V9))

# post
post_adapt_no_context = subset(post_adapt_no_context, select= -c(V9))
post_adapt_launch = subset(post_adapt_launch, select= -c(V9))
post_adapt_pass = subset(post_adapt_pass, select= -c(V9))
# complete datasets for pre and post adaptation

# Pre-Adaptation PMF ----------------------------------------------------------

colnames(pre_adapt_launch)[1] = "discs"
colnames(pre_adapt_launch)[2] = "answers" 

colnames(pre_adapt_no_context)[1] ="discs"
colnames(pre_adapt_no_context)[2] ="answers"

colnames(pre_adapt_pass)[1] ="discs"
colnames(pre_adapt_pass)[2] ="answers" 

launch_sorted = arrange(pre_adapt_launch, discs)

no_context_sorted = arrange(pre_adapt_no_context, discs)

pass_sorted = arrange(pre_adapt_pass, discs)

# psychometric fitting
pmf_launch = quickpsy(launch_sorted,discs,answers,guess=0,lapses=FALSE,prob=0.5,fun=logistic_fun2,parini=list(c(1,15),c(-15,-1)), bootstrap = "none")
launch_pse = pmf_launch$thresholds # 0.6363651

pmf_pass = quickpsy(pass_sorted,discs,answers,guess=0,lapses=FALSE,prob=0.5,fun=logistic_fun2,parini=list(c(1,15),c(-15,-1)), bootstrap = "none")
pass_pse = pmf_pass$thresholds # 0.46575

pmf_no_context = quickpsy(no_context_sorted,discs,answers,guess=0,lapses=FALSE,prob=0.5,fun=logistic_fun2,parini=list(c(1,15),c(-15,-1)), bootstrap = "none")
no_context_pse = pmf_no_context$thresholds # 0.5416502

xvals <- seq(0,1,length.out=100)

pred_launch <- logistic_fun2(xvals,pmf_launch$par$par)
pred_pass <- logistic_fun2(xvals,pmf_pass$par$par)
pred_no_context <- logistic_fun2(xvals,pmf_no_context$par$par)


tripleplot_data = data.frame(
  xvals = seq(0,1,length.out=100),
  launch_curve = pred_launch,
  pass_curve = pred_pass,
  no_context_curve = pred_no_context
)

pre_plot = tripleplot_data


triple_plot = ggplot(tripleplot_data, aes(x = xvals)) +
  geom_line(aes(y = pred_launch), color = "blue", size = 1.5) + 
  geom_line(aes(y = pred_pass), color = "red", size = 1.5) +
  geom_line(aes(y = pred_no_context), color = "green", size = 1.5) +
  labs(title = "Psychometric Curves Pre Adapt",
       x = "Disc Overlap",
       y = "Proportion Causal Report") +
  xlim(0,1) + 
  ylim(0,1)

PSE_launch = pmf_launch$thresholds$thre
PSE_pass = pmf_pass$thresholds$thre
PSE_no_context = pmf_no_context$thresholds$thre

triple_pse = data.frame(y = 0.5,
                        PSE = c(PSE_launch, PSE_pass, PSE_no_context), 
                        curve_type = c("launch_curve", "pass_curve", "no_context_curve"))
pre_pse = triple_pse
pre_pse$version = "pre"

triple_plot = triple_plot + scale_color_manual(values = c("launch_curve" = "blue", "pass_curve" = "red", "no_context_curve" = "green")) 

triple_plot = triple_plot + geom_point(data = triple_pse, aes(x = PSE, y = y, color = curve_type), size = 3, shape = 21, fill = "white")

print(triple_plot)


# post-adaptation PMF---------------------------------------------------------

colnames(post_adapt_launch)[1] = "discs"
colnames(post_adapt_launch)[2] = "answers" 

colnames(post_adapt_no_context)[1] ="discs"
colnames(post_adapt_no_context)[2] ="answers"

colnames(post_adapt_pass)[1] ="discs"
colnames(post_adapt_pass)[2] ="answers" 

launch_sorted = arrange(post_adapt_launch, discs)

no_context_sorted = arrange(post_adapt_no_context, discs)

pass_sorted = arrange(post_adapt_pass, discs)

# psychometric fitting
pmf_launch = quickpsy(launch_sorted,discs,answers,guess=0,lapses=FALSE,prob=0.5,fun=logistic_fun2,parini=list(c(1,15),c(-15,-1)), bootstrap = "none")
launch_pse = pmf_launch$thresholds # 0.6363651

pmf_pass = quickpsy(pass_sorted,discs,answers,guess=0,lapses=FALSE,prob=0.5,fun=logistic_fun2,parini=list(c(1,15),c(-15,-1)), bootstrap = "none")
pass_pse = pmf_pass$thresholds # 0.46575

pmf_no_context = quickpsy(no_context_sorted,discs,answers,guess=0,lapses=FALSE,prob=0.5,fun=logistic_fun2,parini=list(c(1,15),c(-15,-1)), bootstrap = "none")
no_context_pse = pmf_no_context$thresholds # 0.5416502

xvals <- seq(0,1,length.out=100)

pred_launch <- logistic_fun2(xvals,pmf_launch$par$par)
pred_pass <- logistic_fun2(xvals,pmf_pass$par$par)
pred_no_context <- logistic_fun2(xvals,pmf_no_context$par$par)


tripleplot_data = data.frame(
  xvals = seq(0,1,length.out=100),
  launch_curve = pred_launch,
  pass_curve = pred_pass,
  no_context_curve = pred_no_context
)

post_plot = tripleplot_data

triple_plot = ggplot(tripleplot_data, aes(x = xvals)) +
  geom_line(aes(y = pred_launch), color = "blue", size = 1.5, linetype = "dashed") + 
  geom_line(aes(y = pred_pass), color = "red", size = 1.5, linetype = "dashed") +
  geom_line(aes(y = pred_no_context), color = "green", size = 1.5, linetype = "dashed") +
  labs(title = "Psychometric Curves Post Adapt",
       x = "Disc Overlap",
       y = "Proportion Causal Report") +
  xlim(0,1) + 
  ylim(0,1)

PSE_launch = pmf_launch$thresholds$thre
PSE_pass = pmf_pass$thresholds$thre
PSE_no_context = pmf_no_context$thresholds$thre

triple_pse = data.frame(y = 0.5,
                        PSE = c(PSE_launch, PSE_pass, PSE_no_context), 
                        curve_type = c("launch_curve", "pass_curve", "no_context_curve"))
post_pse = triple_pse
post_pse$version = "post"

combo_pse = rbind(post_pse, pre_pse)

combo_pse$curve_type = as.factor(combo_pse$curve_type)
combo_pse$version = as.factor(combo_pse$version)

#names(combo_pse)[names(combo_pse) == "curve_type"] <- "curve_type"

triple_plot = triple_plot + scale_color_manual(values = c("launch_curve" = "blue", "pass_curve" = "red", "no_context_curve" = "green")) 

triple_plot = triple_plot + geom_point(data = triple_pse, aes(x = PSE, y = y, color = curve_type), size = 3, shape = 21, fill = "white")

print(triple_plot)


# plot all 6 conditions together in one plot  -----------------------------

pre_plot$version = "pre"
post_plot$version = "post"

# Combine both data frames
combined_data = rbind(pre_plot, post_plot)

reshaped_data <- combined_data %>%
  gather(key = "curve_type", value = "y_values", launch_curve, pass_curve, no_context_curve)

reshaped_data$curve_type = as.factor(reshaped_data$curve_type)
reshaped_data$version = as.factor(reshaped_data$version) 
# Create a plot using ggplot2

custom_palette = viridis(n = 3, option = "D", begin = 0.2, end = 0.9)

combo_plot <- ggplot(data = reshaped_data, aes(x = xvals, y = y_values, linetype = version, color = curve_type)) +
  geom_line(size = 1.5) +
  scale_linetype_manual(values = c("pre" = "solid", "post" = "dashed")) +
  labs(title = "Curves for Conditions Pre and Post Adaptation",
       x = "Disc Overlap",
       y = "Proportion Causal Report",
       color = "Condition",
       shape = "Curve Type",
       linetype = "Adaptation") +
  theme_classic() +
  xlim(0,1) + 
  ylim(0,1) +
  scale_colour_manual(values = custom_palette)
  # theme(legend.key.size = unit(0.5, "in"))

combo_plot = combo_plot + geom_point(data = combo_pse, aes(x = PSE, y = y, color = curve_type), size = 3, shape = 21, fill = "white")
# Display the plot
print(combo_plot) 
# ggsave("C:/Users/somme/Desktop/Bachelorarbeit/Figures/comboplot.png", width=8, height=8, units="in")


# blockwise mean launch report rate ---------------------------------------
# V1 codes for block 
# Assuming your main data frame is named 'df'
# You can calculate the mean launch report rate using the 'aggregate' function
# Here, 'V26' is the column that codes for the answer, and 'V1' is the column for block number

# Calculate mean launch report rate per block per person
# Calculate mean launch report rate per block per person
mean_rates <- aggregate(COC_file$V26, by=list(COC_file$V1), FUN=mean)

# Rename the columns for clarity
colnames(mean_rates) <- c("Block", "Mean_Launch_Rate")

# Add the mean of means as a new column in the mean_rates data frame
mean_rates$mean_of_Means = cumsum(mean_rates$Mean_Launch_Rate) / (1:nrow(mean_rates))

# Create a scatterplot of the mean launch report rate
mean_plot = ggplot(mean_rates, aes(x=Block, y=Mean_Launch_Rate)) +
  geom_point() +  # Scatterplot points for individual block means
  geom_line() +   # Connect points with lines
  #geom_point(aes(x=Block, y=mean_of_Means), color="red", size=3) +  # Plot mean of means as red points
  geom_line(aes(x=Block, y=mean_of_Means), color="red") +  # Connect mean of means with a red line
  labs(title="Mean Launch Report Rate per Block",
       x="Block Number", y="Mean Causal Report") +
  theme_minimal()  + ylim(0.35,0.6)

print(mean_plot)

# ggsave("C:/Users/somme/Desktop/Bachelorarbeit/Figures/boomboclat.png", width=8, height=8, units="in")

# ANOVA and TTEST (maybe boxplots?) ---------------------------------------
logistic_fun2 <- function (x, p) {(1 + exp(-(p[1]+p[2]*x)))^(-1)}
discs <- seq(0,1,length.out=7)

ANOVA_df = data.frame(
  pid = numeric(),
  adaptation = character(),
  condition = character(),
  PSE = numeric()     
)
# 10 people; 3 conditions; 2 adaptations(; 2 sessions) -> 60 obs


testdir = list.files("C:/Users/somme/Desktop/Bachelorarbeit/Data/CoCa/Test")
combined_data = data.frame()

pid = 1

for (file in testdir) {
  data = read.table(paste0("C:/Users/somme/Desktop/Bachelorarbeit/Data/CoCa/Test/", file))
  # Check if the number of observations is greater than 672
  if (nrow(data) > 672) {
    data <- data[(nrow(data) - 671):nrow(data), ]
    
  }
  combined_data <- rbind(combined_data, data)
  
  coc_reduced = subset(combined_data, select = c(3,6,9,26))
  
  coc_pre_adapt = coc_reduced[coc_reduced$V3 == 0, ]
  coc_pre_adapt = subset(coc_pre_adapt, select = -c(V3))
  
  coc_post_adapt = coc_reduced[coc_reduced$V3 == 1, ]
  coc_post_adapt = subset(coc_post_adapt, select = -c(V3))
  
  # pre adaptation conditions 
  pre_adapt_no_context = coc_pre_adapt[coc_pre_adapt$V9 == 1, ]
  pre_adapt_launch = coc_pre_adapt[coc_pre_adapt$V9 == 2, ]
  pre_adapt_pass = coc_pre_adapt[coc_pre_adapt$V9 == 4, ]
  
  # post adaptation conditions 
  post_adapt_no_context = coc_post_adapt[coc_post_adapt$V9 == 1,]
  post_adapt_launch = coc_post_adapt[coc_post_adapt$V9 == 2,]
  post_adapt_pass = coc_post_adapt[coc_post_adapt$V9 == 4,]
  
  # reduce V9 six times?
  # pre
  pre_adapt_no_context = subset(pre_adapt_no_context, select= -c(V9))
  pre_adapt_launch = subset(pre_adapt_launch, select= -c(V9))
  pre_adapt_pass = subset(pre_adapt_pass, select= -c(V9))
  
  # post
  post_adapt_no_context = subset(post_adapt_no_context, select= -c(V9))
  post_adapt_launch = subset(post_adapt_launch, select= -c(V9))
  post_adapt_pass = subset(post_adapt_pass, select= -c(V9))
  # complete datasets for pre and post adaptation and conditions
  
  # pre conds 
  colnames(pre_adapt_launch)[1] = "discs"
  colnames(pre_adapt_launch)[2] = "answers" 
  
  colnames(pre_adapt_no_context)[1] ="discs"
  colnames(pre_adapt_no_context)[2] ="answers"
  
  colnames(pre_adapt_pass)[1] ="discs"
  colnames(pre_adapt_pass)[2] ="answers" 
  
  launch_sorted = arrange(pre_adapt_launch, discs)
  
  no_context_sorted = arrange(pre_adapt_no_context, discs)
  
  pass_sorted = arrange(pre_adapt_pass, discs)
  
  # psychometric fitting
  pmf_launch = quickpsy(launch_sorted,discs,answers,guess=0,lapses=FALSE,prob=0.5,fun=logistic_fun2,parini=list(c(1,15),c(-15,-1)), bootstrap = "none")
  launch_pse = pmf_launch$thresholds[1] # 0.6363651
  new_row = data.frame(pid = pid, adaptation = "pre", condition = "launch", PSE = launch_pse)
  ANOVA_df = rbind(ANOVA_df, new_row)
  
  pmf_pass = quickpsy(pass_sorted,discs,answers,guess=0,lapses=FALSE,prob=0.5,fun=logistic_fun2,parini=list(c(1,15),c(-15,-1)), bootstrap = "none")
  pass_pse = pmf_pass$thresholds[1] # 0.46575
  new_row = data.frame(pid = pid, adaptation = "pre", condition = "pass", PSE = pass_pse)
  ANOVA_df = rbind(ANOVA_df, new_row)
  
  pmf_no_context = quickpsy(no_context_sorted,discs,answers,guess=0,lapses=FALSE,prob=0.5,fun=logistic_fun2,parini=list(c(1,15),c(-15,-1)), bootstrap = "none")
  no_context_pse = pmf_no_context$thresholds[1] # 0.5416502
  new_row = data.frame(pid = pid, adaptation = "pre", condition = "no_context", PSE = no_context_pse)
  ANOVA_df = rbind(ANOVA_df, new_row)
  
  # post conds 
  colnames(post_adapt_launch)[1] = "discs"
  colnames(post_adapt_launch)[2] = "answers" 
  
  colnames(post_adapt_no_context)[1] ="discs"
  colnames(post_adapt_no_context)[2] ="answers"
  
  colnames(post_adapt_pass)[1] ="discs"
  colnames(post_adapt_pass)[2] ="answers" 
  
  launch_sorted = arrange(post_adapt_launch, discs)
  
  no_context_sorted = arrange(post_adapt_no_context, discs)
  
  pass_sorted = arrange(post_adapt_pass, discs)
  
  # psychometric fitting
  pmf_launch = quickpsy(launch_sorted,discs,answers,guess=0,lapses=FALSE,prob=0.5,fun=logistic_fun2,parini=list(c(1,15),c(-15,-1)), bootstrap = "none")
  launch_pse = pmf_launch$thresholds[1] # 0.6363651
  #print(launch_pse)
  new_row = data.frame(pid = pid, adaptation = "post", condition = "launch", PSE = launch_pse)
  ANOVA_df = rbind(ANOVA_df, new_row)
  
  pmf_pass = quickpsy(pass_sorted,discs,answers,guess=0,lapses=FALSE,prob=0.5,fun=logistic_fun2,parini=list(c(1,15),c(-15,-1)), bootstrap = "none")
  pass_pse = pmf_pass$thresholds[1] # 0.46575
  new_row = data.frame(pid = pid, adaptation = "post", condition = "pass", PSE = pass_pse)
  ANOVA_df = rbind(ANOVA_df, new_row)

  pmf_no_context = quickpsy(no_context_sorted,discs,answers,guess=0,lapses=FALSE,prob=0.5,fun=logistic_fun2,parini=list(c(1,15),c(-15,-1)), bootstrap = "none")
  no_context_pse = pmf_no_context$thresholds[1]# 0.5416502
  new_row = data.frame(pid = pid, adaptation = "post", condition = "no_context", PSE = no_context_pse)
  ANOVA_df = rbind(ANOVA_df, new_row)
  
  pid = pid+1
}

ANOVA_df$adaptation = as.factor(ANOVA_df$adaptation)
ANOVA_df$condition = as.factor(ANOVA_df$condition)
names(ANOVA_df)[names(ANOVA_df) == 'thre'] <- 'PSE'

# now we have functional anova table 

ANOVA_reshaped = ANOVA_df %>%
  group_by(condition, adaptation) %>%
  get_summary_stats(PSE, type = "mean_sd")

pse_anova <- aov(PSE ~ condition * adaptation + Error(pid), data = ANOVA_df)
anova_result = summary(pse_anova)
print(anova_result)

# pairwise comparisons
pwc <- ANOVA_df %>%
  pairwise_t_test(
    PSE ~ condition, paired = TRUE,
    p.adjust.method = "bonferroni"
  )
pwc
# TODO: 
# Fix Lapse Rate 

