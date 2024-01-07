########################################
### Lab 4 - Gaussian Processes
### By Hannes Bengtsson
### TDDE15 - Advanced Machine Learning
### Linköping University
########################################
# Install and import necessary packages for the lab
library(kernlab)
library(ggplot2)
library(AtmRay)

############################################
################ EXERCISE 1 ###############
###########################################

# Squared exponential kernel function
SEkernel <- function(x1, x2, sigmaf = 1, l = 1) {
  n1 <- length(x1)
  n2 <- length(x2)
  K <- matrix(NA, n1, n2)
  for (i in 1:n2) {
    K[,i] <- sigmaf^2 * exp(-0.5 * ((x1 - x2[i]) / l)^2)
  }
  return(K)
}

# Function simulating the posterior distribution of f
posteriorGP <- function (x, y, xStar, sigmaNoise, k, ...) {
  # Number of observations
  n <- length(x)
  
  # Calculate the covariance matrices:
  # k(X, X), k(X, X*), k(X*, X*)
  K <- k(x, x, ...)
  KStar <- k(x, xStar, ...)
  KStarStar <- k(xStar, xStar, ...)
  
  # Calculate the lower triangular matrix L
  L <- t(chol(K + (sigmaNoise ^ 2) * diag(n)))
  
  # Calculate alpha
  alpha <- solve(t(L), solve(L, y))
  
  # Calculate the posterior mean for f
  f_mean <- t(KStar) %*% alpha
  
  # Compute the posterior covariance matrix of f
  v <- solve(L, KStar)
  f_cov <- KStarStar - t(v) %*% v
  
  # The diagonal in the covariance matrix gives us the variance
  f_var <- diag(f_cov)
  
  # Compute the Log Marginal Likelihood
  logmar <- -0.5 * (t(y) %*% alpha) - sum(log(diag(L))) - (n/2) * log(2*pi)
  
  # Return a vector with the posterior mean and variance of f
  return(list(Mean=f_mean[,1], Variance=f_var, LogMar=as.numeric(logmar)))
}

############################################
################ EXERCISE 2 ###############
###########################################

# Simulating some data
obs <- data.frame(x = 0.4, y = 0.719)
sigmaf <- 1
ell <- 0.3
sigmaNoise <- 0.1

x_interval <- seq(-1,1,length=100)

f_posterior <- posteriorGP(x = obs$x,
                           y = obs$y,
                           xStar = x_interval,
                           sigmaNoise = sigmaNoise,
                           k = SEkernel,
                           sigmaf=sigmaf,
                           l = ell)

# A function to plot our posterior mean with 95% probability bands
plot_post_mean <- function(mean, variance, interval, observations) {
  
  # 95% probability bands for f corresponds to 1.96 standard deviations
  prob_bands <-  list (upper = mean + 1.96 * sqrt(variance),
                       lower = mean - 1.96 * sqrt(variance))
  
  # Set ylim so that we include our probability bands
  ylim <- c(-4,4) # Alt: min(prob_bands$lower) and max(prob_bands$upper)
  
  # Plot the mean
  plot(x = interval,
       y = mean,
       type = 'l',
       col = 'black',
       ylab = 'Y',
       xlab = "X",
       ylim = ylim)
  
  # Draw the probability bands on the plot
  lines(x = interval,
        y = prob_bands$upper,
        col = "blue",
        lwd = 2)
  lines(x = interval,
        y = prob_bands$lower,
        col = "blue",
        lwd = 2)
  
  # Add our observations
  points(x = observations$x,
         y = observations$y,
         col = "black",
         pch = 16)
  
  # Add legend
  legend('topright',
         legend = c("Posterior mean of f", "95% PB for f", "Observations"),
         col = c("black", "blue", "black"),
         lty = c(1, 1, NA),
         lwd = c(1,2, NA),
         pch = c(NA, NA, 16),
         cex = 0.8)
}

plot_post_mean(f_posterior$Mean, f_posterior$Variance,x_interval,obs)

############################################
################ EXERCISE 3 ###############
###########################################

# Update with 2 observations
obs <- data.frame(x = c(0.4,-0.6), y = c(0.719,-0.044))

# Get the posterior mean and variance of f
f_posterior <- posteriorGP(x = obs$x,
                           y = obs$y,
                           xStar = x_interval,
                           sigmaNoise = sigmaNoise,
                           k = SEkernel,
                           sigmaf = sigmaf,
                           l = ell)

# Plot the posterior mean of f, observations, and 95% probability bands for f
plot_post_mean(f_posterior$Mean, f_posterior$Variance, x_interval, obs)

############################################
################ EXERCISE 4 ###############
###########################################

# Update with 5 observations
obs <- data.frame(x = c(-1.0,-0.6,-0.2,0.4,0.8), 
                  y = c(0.768,-0.044,-0.940,0.719,-0.664))

# Get the posterior mean and variance of f
f_posterior <- posteriorGP(x = obs$x,
                           y = obs$y,
                           xStar = x_interval,
                           sigmaNoise = sigmaNoise,
                           k = SEkernel,
                           sigmaf = sigmaf,
                           l = ell)

# Plot the posterior mean of f, observations, and 95% probability bands for f
plot_post_mean(f_posterior$Mean, f_posterior$Variance, x_interval, obs)

############################################
################ EXERCISE 5 ###############
###########################################

# Update parameters
sigmaf <- 1
ell <- 1

# Get the posterior mean and variance of f
f_posterior <- posteriorGP(x = obs$x,
                           y = obs$y,
                           xStar = x_interval,
                           sigmaNoise = sigmaNoise,
                           k = SEkernel,
                           sigmaf = sigmaf,
                           l = ell)

# Plot the posterior mean of f, observations, and 95% probability bands for f
plot_post_mean(f_posterior$Mean, f_posterior$Variance, x_interval, obs)

############################################
################ EXERCISE 6 ###############
###########################################

#Import the dataset
df <-  read.csv("https://github.com/STIMALiU/AdvMLCourse/raw/master/GaussianProcess/Code/TempTullinge.csv",
                header=TRUE, sep=";")

# Create the variable time which records the day number since the start of the dataset
time <- seq(1,2190,1)

# Create the variable day that records the day number since the start of each year 
day <- c(rep(seq(1,365,1),6))

# Get every fifth time and day
observations <- data.frame(temp = df$temp[time[seq(1, length(time), by = 5)]], 
                           time = time[seq(1, length(time), by = 5)],
                           day = day[seq(1, length(day), by = 5)])


# Squared exponential kernel function
SEkernel_nestled <- function(sigmaf = 1, ell = 1) 
{
  rval <- function(x1, x2 = NULL) {
    return(sigmaf^2*exp(-(x1-x2)^2/(2*ell^2)))
  }
  class(rval) <- "kernel"
  return(rval)
} 

SEkernelFunc <- SEkernel_nestled(sigmaf = 1, ell = 1) # Our kernel FUNCTION.
SEkernelFunc(c(1,1),c(2,2)) # Evaluating the kernel in x=c(1,1), x'=c(2,2).

# Testing our own defined kernel function.
X <- matrix(c(1,3,4)) # Simulating some data.
Xstar <- matrix(c(2,3,4))

# Computing the whole covariance matrix K from the kernel.
kernelMatrix(kernel = SEkernelFunc, x = X, y = Xstar) # So this is K(X,Xstar).

############################################
################ EXERCISE 7 ###############
###########################################

# Linear model to obtain sigmaNoise
lmfit <- lm(formula = temp ~ time + I(time^2), data = observations)
sigmaNoise <- sd(lmfit$residuals)

# Set the parameters for our kernel function
SEkernelFunc <- SEkernel_nestled(sigmaf = 20, ell = 0.2)

# Fit the GP with our SEkernel function
GPfit <- gausspr(y = observations$temp,
                 x = observations$time, 
                 kernel = SEkernelFunc, 
                 var = sigmaNoise ^ 2)

# Predicting the mean for the training data
pred.temp.mean <- predict(GPfit, observations$time) 

# Plot our observations and the posterior mean of f
plot(observations$time, observations$temp, xlab = "Time", ylab = "Temperature", 
     pch = 1, main = "GP Regression, function of time, SEkernel")
lines(observations$time, pred.temp.mean, type = "l", col = "red")
legend('bottomright',
       legend = c("Posterior mean of f", "Observations"),
       col = c("red","black"),
       lty = c(1, NA),
       pch = c(NA, 1),
       cex = 0.8)

############################################
################ EXERCISE 8 ###############
###########################################

# Initiate parameters
sigmaf = 20
ell = 0.2

#Scale the x and y -> (val-mean)/sd
scaled.time <- scale(observations$time)
scaled.temp <- scale(observations$temp)

# Linear model to get the new noise for scaled data
lmfit.scaled <- lm(scaled.temp ~ scaled.time + I(scaled.time^2))
sigmaNoise.scaled <- var(lmfit.scaled$residuals)

# Get the posterior mean and variance of f
f_posterior <- posteriorGP(x = scaled.time,
                           y = scaled.temp,
                           xStar = scaled.time,
                           sigmaNoise = sigmaNoise.scaled,
                           k = SEkernel,
                           sigmaf = sigmaf,
                           l = ell)

# Here we use the mean obtained from the gausspr package and unscale our 
# standard deviation to get the upper and lower probability bands for f
upper_bound <-  pred.temp.mean + 1.96 * sqrt(f_posterior$Variance)*sd(observations$temp)
lower_bound <-  pred.temp.mean - 1.96 * sqrt(f_posterior$Variance)*sd(observations$temp)

# Plot the posterior mean of f, observations, and 95% probability bands for f
plot(x = observations$time, 
     y = observations$temp, 
     xlab = "Time", 
     ylab = "Temperature", 
     pch = 1, 
     main = "GP regression, function of time, SEkernel",
     ylim = c(-22,22))
#ylim = c(min(lower_bound)-1,max(upper_bound)+1)
lines(observations$time, pred.temp.mean, type = "l", col = "red")
lines(observations$time, upper_bound, type = "l", col = "blue")
lines(observations$time, lower_bound, type = "l", col = "blue")
# Add legend
legend('bottomright', 
       legend = c("Posterior mean of f", "95% PB of f", "Observations"),
       col = c("red", "blue", "black"),
       lty = c(1, 1, NA),
       lwd = c(1, 2, NA),
       pch = c(NA, NA, 16),
       cex = 0.8)

############################################
################ EXERCISE 9 ###############
###########################################

# Linear model to obtain sigmaNoise
lmfit <- lm(formula = temp ~ day + I(day^2), data = observations)
sigmaNoise <- sd(lmfit$residuals)

# Set the parameters for our kernel function
SEkernelFunc <- SEkernel_nestled(sigmaf = 20, ell = 0.2)

# Fit the GP with our SEkernel function
GPfit <- gausspr(y = observations$temp,
                 x = observations$day, 
                 kernel = SEkernelFunc, 
                 var = sigmaNoise ^ 2)

# Predicting the training data
pred.tempday.mean <- predict(GPfit, observations$day) 

# Plot our observations and the posterior mean of f
plot(observations$time, observations$temp, xlab = "Time", ylab = "Temperature", 
     pch = 1, main = "GP using time and day")
lines(observations$time, pred.temp.mean, type = "l", col = "red")
lines(observations$time, pred.tempday.mean, type = "l", col = "blue")
# Add legend
legend('bottomright',
       title = "Posterior mean of f",
       legend = c("Time model", "Day model", "Observations"),
       col = c("red", "blue", "black"),
       lty = c(1, 1, NA),
       lwd = c(1, 2, NA),
       pch = c(NA, NA, 16),
       cex = 0.8)

############################################
################ EXERCISE 10 ##############
###########################################

# Logically periodic kernel function
LPkernel <- function(sigmaf = 1, ell1 = 1, ell2 = 1, d = 1) 
{
  rval <- function(x1, x2 = NULL) {
    return(sigmaf ^2 * exp(-2 * (sin(pi * abs(x1 - x2) / d)) ^ 2 / ell1 ^ 2)
           * exp(-1/2 * abs(x1 - x2) ^ 2 / ell2 ^ 2))
  }
  class(rval) <- "kernel"
  return(rval)
} 

sigmaf <- 20
ell1 <- 1
ell2 <- 10
d <- 365/sd(observations$time)

# Linear model to obtain sigmaNoise
lmfit <- lm(formula = temp ~ time + I(time^2), data = observations)
sigmaNoise <- sd(lmfit$residuals)

# Set the parameters for our kernel function
kernelFunc <- LPkernel(sigmaf = sigmaf, ell1 = ell1, ell2 = ell2, d = d)

# Fit the GP with our SEkernel function
GPfit <- gausspr(y = observations$temp,
                 x = observations$time, 
                 kernel = kernelFunc, 
                 var = sigmaNoise ^ 2)

# Predicting the training data
pred.mean.SE3 <- predict(GPfit, observations$time) 

# Plot our observations and the posterior mean of f
plot(observations$time, observations$temp, xlab = "Time", ylab = "Temperature", 
     pch = 1, main = "GP Regression, Locally Periodic Kernel")
lines(observations$time, pred.mean.SE3, type = "l", col = "green")
lines(observations$time, pred.tempday.mean, type = "l", col = "blue")
lines(observations$time, pred.temp.mean, type = "l", col = "red")
# Add legend
legend('bottomright',
       title = "Posterior mean of f",
       legend = c("Time LPkernel", "Day SEkernel", "Time SEkernel", "Observations"),
       col = c("green", "blue", "red","black"),
       lty = c(1, 1, 1, NA),
       pch = c(NA, NA, NA, 1),
       cex = 0.8)


############################################
################ EXERCISE 11 ##############
###########################################

data <- read.csv("https://github.com/STIMALiU/AdvMLCourse/raw/master/GaussianProcess/Code/banknoteFraud.csv",
                 header=FALSE, sep=",")
names(data) <- c("varWave","skewWave","kurtWave","entropyWave","fraud") 
data[,5] <- as.factor(data[,5])

set.seed(111); 
SelectTraining <- sample(1:dim(data)[1], size = 1000, replace = FALSE)

# Divide the data
train <- data[SelectTraining,]
test <- data[-SelectTraining,]

# Fit the GP using 2 covariates varWave and skeWave
GPfitfraud <- gausspr(fraud ~  varWave + skewWave, data=train)

# Create intervals for x1 and x2 that captures all values in the training data
x1 <- seq(min(train$varWave),max(train$varWave),length=100)
x2 <- seq(min(train$skewWave),max(train$skewWave),length=100)

# Prepare data for plot
gridPoints <- meshgrid(x1, x2)
gridPoints <- cbind(c(gridPoints$x), c(gridPoints$y))

gridPoints <- data.frame(gridPoints)
names(gridPoints) <- names(train[1:2])
probPreds <- predict(GPfitfraud, gridPoints, type="probabilities")

# Plotting 
contour(x1,x2,matrix(probPreds[,2],100,byrow = TRUE), 20, xlab = "varWave", 
        ylab = "skewWave", main = "GP Fraud Classification")
points(train[train[,5]==1,1],train[train[,5]==1,2],col="blue")
points(train[train[,5]==0,1],train[train[,5]==0,2],col="red")

# Add legend
legend('bottomright',
       legend = c("1", "0"),
       col = c("blue", "red"),
       pch = c(1, 1),
       cex = 0.8)


# Accuracy function
accuracy <- function(X,X1){
  n <- length(X)
  return(sum(diag(table(X,X1)))/n)
}

# Confusion matrix for the training data
pred <- predict(GPfitfraud, newdata=train)
table(pred, train$fraud)
accuracy(pred, train$fraud) # Accuracy

# Confusion matrix for the test data
pred <- predict(GPfitfraud, newdata=test)
table(pred, test$fraud)
accuracy(pred, test$fraud) # Accuracy

############################################
################ EXERCISE 12 ##############
###########################################

GPfitfraud <- gausspr(fraud ~., data=train)

gridPoints <- data.frame(gridPoints)
names(gridPoints) <- names(train[1:2])
probPreds <- predict(GPfitfraud, type="probabilities")

# Confusion matrix for the training data
pred <- predict(GPfitfraud, newdata=train)
table(pred, train$fraud)
accuracy(pred, train$fraud) # Accuracy

# Confusion matrix for the test data
pred <- predict(GPfitfraud, newdata=test)
table(pred, test$fraud)
accuracy(pred, test$fraud) # Accuracy