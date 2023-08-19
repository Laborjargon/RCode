########################################
# Bachelor thesis CoCa Analysis Script #
########################################
# data in the same plot 
library(ggplot2)
library(dplyr)
library(quickpsy)
library(reshape)
library(tidyr)

logistic_fun2 <- function (x, p) {(1 + exp(-(p[1]+p[2]*x)))^(-1)}
discs <- seq(0,1,length.out=7)

COC_file = read.table("C:/Users/somme/Desktop/Bachelorarbeit/Data/CoCa/Coc1093.dat")
# depending on working computer:
# COC_file = read.table("C:/Users/Ben/Desktop/RSHIT/Data/COC/Coc1011.dat")
# split data into four conditions 1 no_context 2 launch 4 pass 

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
combo_plot <- ggplot(data = reshaped_data, aes(x = xvals, y = y_values, linetype = version, color = curve_type    )) +
  geom_line() +
  scale_linetype_manual(values = c("pre" = "solid", "post" = "dashed")) +
  labs(title = "Curves for Pre and Post Conditions",
       x = "X Axis Label",
       y = "Y Axis Label",
       color = "Version",
       shape = "Curve Type",
       linetype = "Version") +
  theme_minimal()

# Display the plot
print(combo_plot)
