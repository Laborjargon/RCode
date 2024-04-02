load("C:/Users/Ben/Downloads/PSE_table.RData")

sample_mean <- mean(PSE_table$thre)
sample_sd <- sd(PSE_table$thre)

pid_sd <- aggregate(thre ~ pid, data = PSE_table, FUN = sd)
pid_mean <- aggregate(thre ~ pid, data = PSE_table, FUN = mean)

condition_sd <- aggregate(thre ~ conditionlong, data = PSE_table, FUN = sd)
condition_means <- aggregate(thre ~ conditionlong, data = PSE_table, FUN = mean)

av_within_sd <- mean(pid_sd$thre)
av_within_mean <- mean(pid_mean$thre) # equal to sample mean

dPSE <- data.frame(launchTest = PSE_table[PSE_table$conditionlong == "launch_post_test",]$thre
                   - PSE_table[PSE_table$conditionlong == "launch_pre",]$thre,
              
                   launchCont = PSE_table[PSE_table$conditionlong == "launch_post_context",]$thre
                   - PSE_table[PSE_table$conditionlong == "launch_pre",]$thre,
                   
                   passTest = PSE_table[PSE_table$conditionlong == "pass_post_test",]$thre
                   - PSE_table[PSE_table$conditionlong == "pass_pre",]$thre,
                   
                   passCont = PSE_table[PSE_table$conditionlong == "pass_post_context",]$thre
                   - PSE_table[PSE_table$conditionlong == "pass_pre",]$thre,
                   
                   nConTest = PSE_table[PSE_table$conditionlong == "no_context_post_test",]$thre
                   - PSE_table[PSE_table$conditionlong == "no_context_pre",]$thre,
                   
                   nConCont = PSE_table[PSE_table$conditionlong == "no_context_post_context",]$thre
                   - PSE_table[PSE_table$conditionlong == "no_context_pre",]$thre)

means <- colMeans(dPSE)
dPSE <- rbind(dPSE, means)

