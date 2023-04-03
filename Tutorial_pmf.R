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
# Welche Werte für die Parameter hat die quickpsy function errechnet?

pmf$par$par[1]
pmf$par$par[2] # nicht gleich weil normalverteilung mit 20 wdh um parameter

# Sind das die gleichen Zahlen, die Du auch für die Simulation genutzt hast?
  # theoretisch sogar ja weil man n -> inf dann sollten sie konvergieren
# Wenn ja, oder nein warum? 
 # s. o. 
# Falls nein, welche Zeile im Script könntest Du anpassen um auf sehr ähnliche Zahlen zu kommen?
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
# shift parameter sch�tzen lassen?
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
# -----------------------------------------------------------------------------