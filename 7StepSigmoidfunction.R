
# main plot ---------------------------------------------------------------
# parameters of the sigmoid curve
slope = -10
midpoint = 0.5

# 7 random, seeded vals between 0 and 1 
set.seed(123)  # is nett zu haben
rand_x = runif(7)

# calculate y-values for sigmoid function
rand_y = 1 / (1 + exp(-slope*(rand_x - midpoint)))

# plot data points
plot(rand_x, rand_y, pch = 13, col = "red", xlab = "Overlap%", ylab = "Report Launch%")


# plot curve and data points ----------------------------------------------

x = seq(0, 1, length.out = 100)
y = 1 / (1 + exp(-slope*(x - midpoint)))

plot(x, y, type = "l", xlab = "Overlap%", ylab = "Report Launch%")
points(rand_x, rand_y, col = "red")


# iterate over 5 seeds constant slope and midpoint ----------------------------------------------------

seeds = sample(1:10000, 5)
slope = -10
midpoint = 0.5

# plot layout
par(mfrow = c(2, 3), mar = c(4, 4, 2, 2), oma = c(0, 0, 2, 0))

for (i in 1:length(seeds)){
  set.seed(seeds[i])
  rand_x = runif(7)
  rand_y = 1 / (1 + exp(-slope*(rand_x - midpoint)))
  plot(rand_x, rand_y, pch = 19, col = "red", 
       xlab = "Overlap%", ylab = "Report Launch%",
       main = paste0("Seed = ", seeds[i]))
  
}


# final section, randomised everything -------------------------------------

seeds = sample(1:10000, 5)

# plot layout
par(mfrow = c(2, 3), mar = c(4, 4, 2, 2), oma = c(0, 0, 2, 0))

for (i in 1:length(seeds)){
  set.seed(seeds[i])
  slope = runif(1, -20, -1)
  midpoint = runif(1)
  rand_x = runif(7)
  rand_y = 1 / (1 + exp(-slope*(rand_x - midpoint)))
  plot(rand_x, rand_y, pch = 19, col = "red", 
       xlab = "Overlap%", ylab = "Report Launch%",
       main = paste0("Seed = ", seeds[i]),
       xlim = c(0, 1),
       ylim = c(0, 1))
  
}

# superfinal section: including the corresponding function ----------------
seeds = sample(1:10000, 5)

# plot layout
par(mfrow = c(2, 3), mar = c(4, 4, 2, 2), oma = c(0, 0, 2, 0))

# for loop with randomised parameters
for (i in 1:length(seeds)){
  set.seed(seeds[i])
  slope = runif(1, -20, -1)
  midpoint = runif(1)
  rand_x = runif(7)
  rand_y = 1 / (1 + exp(-slope*(rand_x - midpoint)))
  cont_x = seq(0, 1, length.out = 100)
  cont_y = 1 / (1 + exp(-slope*(cont_x - midpoint)))
  plot(cont_x, cont_y,
       type = "l",
       xlab = "Overlap%",
       ylab = "Report Launch%",
       main = paste0("Slope = ", round(slope, 2), "\nMidpoint = ", round(midpoint, 2)))
  points(rand_x, rand_y, col = "red",
         xlim = c(0, 1),
         ylim = c(0, 1))
}

