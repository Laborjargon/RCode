# chapter 1: general results 
# 6 curves plot a: pre conditions and post at test w/ corresponding scatterplot
rm(list = ls())

library(ggplot2)
library(dplyr)
library(quickpsy)
library(reshape)
library(tidyr)
library(viridis)

combined_data = data.frame()
combined_data_reduced = data.frame()
data = data.frame()
testdir = list.files("C:/Users/somme/Desktop/Bachelorarbeit/Data/CoCa/Total")
# 9 people; 3 conditions; 2 adaptations(; 2 sessions) -> 54 obs
pid = 1
counter = 1
for (file in testdir) {
  data = read.table(paste0("C:/Users/somme/Desktop/Bachelorarbeit/Data/CoCa/Total/", file))
  # Check if the number of observations is greater than 672
  if (nrow(data) > 672) {
    data <- data[(nrow(data) - 671):nrow(data), ]
  }
  data$pid = pid
  combined_data <- rbind(combined_data, data)
  if (counter %% 2 == 0){
    pid = pid+1
  }
  counter = counter+1 
}

combined_data_reduced = subset(combined_data, select = c(3,6,9,12,26, pid))

# complete datasets for pre and post adaptation and conditions

colnames(combined_data_reduced)[colnames(combined_data_reduced) == "V3"] ="adaptation" # 0: pre, 1: post
colnames(combined_data_reduced)[colnames(combined_data_reduced) == "V6"] ="discs"
colnames(combined_data_reduced)[colnames(combined_data_reduced) == "V9"] ="condition" # 1: No_Context, 2: Launch, 4: Pass
colnames(combined_data_reduced)[colnames(combined_data_reduced) == "V12"] ="location" # 1: Test -1: Context
colnames(combined_data_reduced)[colnames(combined_data_reduced) == "V26"] ="answers"

combined_data_reduced$adaptation = as.factor(combined_data_reduced$adaptation)
combined_data_reduced$condition = as.factor(combined_data_reduced$condition)
combined_data_reduced$location = as.factor(combined_data_reduced$location)


logistic_fun2 <- function (x, p) {(1 + exp(-(p[1]+p[2]*x)))^(-1)}
xvals = seq(0,1,length.out=100)

# condition 1:2:4; adaptation 0:1; location 1:-1
C1A0L12 = which((combined_data_reduced$condition == 1 & combined_data_reduced$adaptation == 0 & combined_data_reduced$location == 1) | (combined_data_reduced$condition == 1 & combined_data_reduced$adaptation == 0 & combined_data_reduced$location == -1))
C1A1L1 = which((combined_data_reduced$condition == 1 & combined_data_reduced$adaptation == 1 & combined_data_reduced$location == 1))
C1A1L2 = which((combined_data_reduced$condition == 1 & combined_data_reduced$adaptation == 1 & combined_data_reduced$location == -1))


C2A0L12 = which((combined_data_reduced$condition == 2 & combined_data_reduced$adaptation == 0 & combined_data_reduced$location == 1) | (combined_data_reduced$condition == 2 & combined_data_reduced$adaptation == 0 & combined_data_reduced$location == -1))
C2A1L1 = which((combined_data_reduced$condition == 2 & combined_data_reduced$adaptation == 1 & combined_data_reduced$location == 1))
C2A1L2 = which((combined_data_reduced$condition == 2 & combined_data_reduced$adaptation == 1 & combined_data_reduced$location == -1))

C3A0L12 = which((combined_data_reduced$condition == 4 & combined_data_reduced$adaptation == 0 & combined_data_reduced$location == 1) | (combined_data_reduced$condition == 4 & combined_data_reduced$adaptation == 0 & combined_data_reduced$location == -1))
C3A1L1 = which((combined_data_reduced$condition == 4 & combined_data_reduced$adaptation == 1 & combined_data_reduced$location == 1)) 
C3A1L2 = which((combined_data_reduced$condition == 4 & combined_data_reduced$adaptation == 1 & combined_data_reduced$location == -1))

combined_data_reduced$conditionlong <- 999 # FILL

combined_data_reduced$conditionlong[C1A0L12] <- "no_context_pre" # "no_context_pre"
combined_data_reduced$conditionlong[C1A1L1] <- "no_context_post_test"  # "no_context_post_test"
combined_data_reduced$conditionlong[C1A1L2] <- "no_context_post_context" # "no_context_post_context"

combined_data_reduced$conditionlong[C2A0L12] <- "launch_pre"
combined_data_reduced$conditionlong[C2A1L1] <- "launch_post_test"
combined_data_reduced$conditionlong[C2A1L2] <- "launch_post_context"

combined_data_reduced$conditionlong[C3A0L12] <- "pass_pre"
combined_data_reduced$conditionlong[C3A1L1] <- "pass_post_test"
combined_data_reduced$conditionlong[C3A1L2] <- "pass_post_context"

combined_data_reduced$conditionlong = as.factor(combined_data_reduced$conditionlong)

regrouped_pmf = quickpsy(combined_data_reduced,discs,answers,guess=0,lapses=F,prob=0.5,grouping = .("conditionlong","pid"), fun=logistic_fun2,parini=list(c(1,15),c(-40,-1)), bootstrap = "none")

# pmfs for conditions location == 1 : test
pmf_launch_pre = quickpsy(combined_data_reduced[combined_data_reduced$condition == 2 & combined_data_reduced$adaptation == 0,],discs,answers,guess=0,lapses=FALSE,prob=0.5,fun=logistic_fun2,parini=list(c(1,15),c(-15,-1)), bootstrap = "none")

pmf_pass_pre = quickpsy(combined_data_reduced[combined_data_reduced$condition == 4 & combined_data_reduced$adaptation == 0,],discs,answers,guess=0,lapses=FALSE,prob=0.5,fun=logistic_fun2,parini=list(c(1,15),c(-15,-1)), bootstrap = "none")

pmf_no_context_pre = quickpsy(combined_data_reduced[combined_data_reduced$condition == 1 & combined_data_reduced$adaptation == 0,],discs,answers,guess=0,lapses=FALSE,prob=0.5,fun=logistic_fun2,parini=list(c(1,15),c(-15,-1)), bootstrap = "none")
# post 
pmf_launch_post = quickpsy(combined_data_reduced[combined_data_reduced$condition == 2 & combined_data_reduced$adaptation == 1 & combined_data_reduced$location == 1,],discs,answers,guess=0,lapses=FALSE,prob=0.5,fun=logistic_fun2,parini=list(c(1,15),c(-15,-1)), bootstrap = "none")

pmf_pass_post = quickpsy(combined_data_reduced[combined_data_reduced$condition == 4 & combined_data_reduced$adaptation == 1 & combined_data_reduced$location == 1,],discs,answers,guess=0,lapses=FALSE,prob=0.5,fun=logistic_fun2,parini=list(c(1,15),c(-15,-1)), bootstrap = "none")

pmf_no_context_post = quickpsy(combined_data_reduced[combined_data_reduced$condition == 1 & combined_data_reduced$adaptation == 1 & combined_data_reduced$location == 1,],discs,answers,guess=0,lapses=FALSE,prob=0.5,fun=logistic_fun2,parini=list(c(1,15),c(-15,-1)), bootstrap = "none")

pred_launch_pre <- logistic_fun2(xvals,pmf_launch_pre$par$par)
pred_pass_pre <- logistic_fun2(xvals,pmf_pass_pre$par$par)
pred_no_context_pre <- logistic_fun2(xvals,pmf_no_context_pre$par$par)

pred_launch_post <- logistic_fun2(xvals,pmf_launch_post$par$par)
pred_pass_post <- logistic_fun2(xvals,pmf_pass_post$par$par)
pred_no_context_post <- logistic_fun2(xvals,pmf_no_context_post$par$par) 

# collecting PSEs 
pre_pse = data.frame(
  y = 0.5,
  launch = pmf_launch_pre$thresholds$thre,
  pass = pmf_pass_pre$thresholds$thre,
  no_context = pmf_no_context_pre$thresholds$thre,
  adaptation = "pre"
)

post_pse = data.frame(
  y = 0.5,
  launch = pmf_launch_post$thresholds$thre,
  pass = pmf_pass_post$thresholds$thre, 
  no_context = pmf_no_context_post$thresholds$thre, 
  adaptation = "post"
)

# getting the plot data
pre_plot_data = data.frame(
  xvals = seq(0,1,length.out=100),
  launch = pred_launch_pre,
  pass = pred_pass_pre,
  no_context = pred_no_context_pre,
  adaptation = "pre"
)

post_plot_data = data.frame(
  xvals = seq(0,1,length.out=100),
  launch = pred_launch_post,
  pass = pred_pass_post,
  no_context = pred_no_context_post,
  adaptation = "post"
)

general_plot_data = rbind(pre_plot_data, post_plot_data)
# PSEs 
general_pse = rbind(pre_pse, post_pse)

# setting things up for the plot
reshaped_data <- general_plot_data %>%
  gather(key = "condition", value = "y_values", launch, pass, no_context)

reshaped_PSE <- general_pse %>%
  gather(key = "condition", value = "PSE", launch, pass, no_context)

reshaped_data$condition = as.factor(reshaped_data$condition)
reshaped_data$adaptation = as.factor(reshaped_data$adaptation)

reshaped_PSE$condition = as.factor(reshaped_PSE$condition)
reshaped_PSE$adaptation = as.factor(reshaped_PSE$adaptation)

custom_palette = viridis(n = 3, option = "D", begin = 0.2, end = 0.9)

general_plot <- ggplot(data = reshaped_data, aes(x = xvals, y = y_values, linetype = adaptation, color = condition)) +
  geom_line(size = 1.5) +
  scale_linetype_manual(values = c("pre" = "solid", "post" = "dashed")) +
  labs(title = "Curves for Conditions Pre and Post Adaptation 1",
       x = "Disc Overlap",
       y = "Proportion Causal Report",
       color = "Condition",
       shape = "Curve Type",
       linetype = "Adaptation") +
  theme_classic() +
  xlim(0,1) + 
  ylim(0,1) +
  scale_colour_manual(values = custom_palette) + 
  geom_point(data = reshaped_PSE, aes(x = PSE, y = y, color = condition), size = 3, shape = 21, fill = "white")

# Display the plot
#print(general_plot)

# adding the points

pre_melt1 <- melt(combined_data_reduced[combined_data_reduced$adaptation == 0,],id=c("discs","condition","pid"),measure=c("answers"))
pre_cast1 <- cast(pre_melt1,discs+condition+pid ~ variable,mean)

pre_melt2 <- melt(pre_cast1,id=c("discs","condition"),measure=c("answers"))
pre_cast2 <- cast(pre_melt2,discs+condition ~ variable,mean) # this is the important one

#ggplot(pre_cast2,aes(x=discs,y=answers,color=condition))+geom_point(size = 2.5)

# post
post_melt1 <- melt(combined_data_reduced[combined_data_reduced$adaptation == 1 & combined_data_reduced$location == 1 ,],id=c("discs","condition","pid"),measure=c("answers"))
post_cast1 <- cast(post_melt1,discs+condition+pid ~ variable,mean)

post_melt2 <- melt(post_cast1,id=c("discs","condition"),measure=c("answers"))
post_cast2 <- cast(post_melt2,discs+condition ~ variable,mean) # this is the important one

# add adaptation markers
post_cast2$adaptation = "post"
pre_cast2$adaptation = "pre"

post_cast2$condition = as.factor(post_cast2$condition)
post_cast2$adaptation = as.factor(post_cast2$adaptation)

pre_cast2$condition = as.factor(pre_cast2$condition)
pre_cast2$adaptation = as.factor(pre_cast2$adaptation)

post_cast2 <- post_cast2 %>%
  mutate(condition = case_when(
    condition == 1 ~ "no_context",
    condition == 2 ~ "launch",
    condition == 4 ~ "pass",
    TRUE ~ as.character(condition)
  ))

#ggplot(post_cast2,aes(x=discs,y=answers,color=condition))+geom_point(size = 2.5)

pre_cast2 <- pre_cast2 %>%
  mutate(condition = case_when(
    condition == 1 ~ "no_context",
    condition == 2 ~ "launch",
    condition == 4 ~ "pass",
    TRUE ~ as.character(condition)
  ))

combined_plot = general_plot + geom_point(data = post_cast2, aes(x = discs, y = answers, color = condition), size = 1.5, shape = 13)+
  geom_point(data = pre_cast2, aes(x = discs, y = answers, color = condition), size = 1.5, shape = 19)

# Display the plot
print(combined_plot)

PSE_table = regrouped_pmf$thresholds

rows_to_remove = c("launch_post_context", "no_context_post_context", "pass_post_context")
PSE_table = PSE_table[!(PSE_table$conditionlong %in% rows_to_remove), ]

#result <- combined_data_grouped %>%
# summarize(mean_value = mean(answers))
means <- aggregate(thre ~ conditionlong, data = PSE_table, FUN = mean)
sd = aggregate(thre ~ conditionlong, data = PSE_table, FUN = sd)
meansd = cbind(means,sd)
meansd <- meansd[, -3]
colnames(meansd)[3] <- "sd"

CIUp =  data.frame(CIUp = meansd$thre + qt(p = .95, df = 8) * meansd$sd / sqrt(9))
CILo = data.frame(CILo = meansd$thre - qt(p = .95, df = 8) * meansd$sd / sqrt(9))

CIUpM =  data.frame(CIUpM = meansd$thre + qt(p = .95, df = 8) * meansd$sd / sqrt(9) * sqrt(9/(9-1)))
CILoM = data.frame(CILoM = meansd$thre - qt(p = .95, df = 8) * meansd$sd / sqrt(9) * sqrt(9/(9-1)))

CI = cbind(CIUp, CILo)
CI$conditionlong = meansd$conditionlong
CI$thre = means$thre

CI = cbind(CI, CIUpM)
CI = cbind(CI, CILoM)

PSE_table$conditionlong <- factor(PSE_table$conditionlong, levels = c("pass_pre", "no_context_pre", "launch_pre",
                                                                      "pass_post_test", "no_context_post_test", "launch_post_test"))

means$conditionlong <- factor(means$conditionlong, levels = c("pass_pre", "no_context_pre", "launch_pre",
                                                              "pass_post_test", "no_context_post_test", "launch_post_test"))


ggplot(data = PSE_table, aes(x = conditionlong, y = thre, color = factor(conditionlong))) +
  geom_point(position = position_jitter(width = 0.05, height = 0), size = 2.5) +
  labs(x = NULL, y = "PSE") +
  theme_minimal() +
  ylim(0,1) + 
  geom_point(data = means, aes(x = conditionlong, y = thre, fill = factor(conditionlong)), size = 3.5, shape= 22, color="black") +
  geom_errorbar(data = CI, aes(ymin = CILo, ymax = CIUp), width = 0.2, color = "black") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  guides(fill = "none",
         color = "none")
# Reihenfolge 

