##########################
# analysis script coca 1 #
##########################

library(ggplot2)
library(dplyr)
library(quickpsy)
library(reshape)
library(tidyr)
library(viridis)
library(stats)

combined_data = data.frame()
combined_data_reduced = data.frame()
data = data.frame()
testdir = list.files("C:/Users/somme/Desktop/Bachelorarbeit/Data/CoCa/Total")
# 10 people; 3 conditions; 2 adaptations(; 2 sessions) -> 60 obs
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

colnames(combined_data_reduced)[colnames(combined_data_reduced) == "V3"] ="adaptation"
colnames(combined_data_reduced)[colnames(combined_data_reduced) == "V6"] ="discs"
colnames(combined_data_reduced)[colnames(combined_data_reduced) == "V9"] ="condition"
colnames(combined_data_reduced)[colnames(combined_data_reduced) == "V12"] ="location"
colnames(combined_data_reduced)[colnames(combined_data_reduced) == "V26"] ="answers"

combined_data_reduced$adaptation = as.factor(combined_data_reduced$adaptation)
combined_data_reduced$condition = as.factor(combined_data_reduced$condition)
combined_data_reduced$location = as.factor(combined_data_reduced$location)


m1 <- melt(combined_data_reduced,id=c("discs","condition","pid"),measure=c("answers"))
c1 <- cast(m1,discs+condition+pid ~ variable,mean)

m2 <- melt(c1,id=c("discs","condition"),measure=c("answers"))
c2 <- cast(m2,discs+condition ~ variable,mean)

ggplot(c1,aes(x=discs,y=answers,color=condition))+geom_point()+geom_line() +facet_wrap(~ pid)