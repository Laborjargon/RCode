
load(file="C:/Users/Ben/Desktop/RSHIT/Data/gpa.rda")

# data structure
head(gpa,10)
str(gpa)
summary(gpa)

# multiregression model
M <- lm(gpa ~ anxiety + study_hours + sleep_hours, data = gpa)
summary(M) # anxiety and intercept significant

# centered predictors 
gpa$anxiety_c <- scale(gpa$anxiety, center = TRUE, scale = FALSE)
gpa$study_hours_c <- scale(gpa$study_hours, center = TRUE, scale = FALSE)
gpa$sleep_hours_c <- scale(gpa$sleep_hours, center = TRUE, scale = FALSE)

# same soup different spices
M_centered <- lm(formula = gpa ~ anxiety_c + study_hours_c + sleep_hours_c, data = gpa)
summary(M_centered)

gpa[, c("anxiety", "study_hours", "sleep_hours")] # estimating regression coefficients
cbind(1, gpa[, c("anxiety", "study_hours", "sleep_hours")]) # why we do this?

# alll the variables we might need
X = as.matrix(cbind(1, gpa[, c("anxiety", "study_hours", "sleep_hours")]))
y = as.matrix(gpa$gpa) # criterion

b_hat = solve(t(X) %*% X) %*% t(X) %*% y
y_hat = X %*% b_hat
e_hat =  y - y_hat # los residuales

N <- nrow(gpa) # Number of individuals
K <- length(coef(M)) - 1 # Number of regression coefficients without counting the intercept

sigma2 <- as.numeric(t(e_hat) %*% e_hat / (N - K - 1)) # Squared standard error of the estimate and convert to scalar
se <- sqrt(diag(sigma2 * solve(t(X) %*% X))) # standard errors
sigma_hat = sqrt(sigma2)

t.test()
# .9 CI 
confint(M, level = 0.9)
confint(M_centered, level = 0.9)


