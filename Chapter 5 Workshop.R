# ````````````````Chapter 5 Lab````````````````
# Lab: Cross-Validation and the Bootstrap 
  # 5.3.1 The Validation Set Approach
# use of the validation set approach in order to estimate the test error rates that result from fitting various linear models
# the sample() function to split the set of observations into two halves, by selecting a random subset of 196 observations out of sample() the original 392 observations. 
library(ISLR)
set.seed(1)
train=sample(392,196)

# the subset option in lm() to fit a linear regression using only the observations corresponding to the training set
lm.fit=lm(mpg~horsepower,data=Auto,subset=train)

# the predict() function to estimate the response for all 392 observations
# the mean() function to calculate the MSE of the 196 observations in the validation set
# the -train index below selects only the observations that are not in the training set
attach(Auto)
mean((mpg-predict(lm.fit,Auto))[-train]^2)
# the estimated test MSE for the linear regression fit is 23.27

# the poly() function to estimate the test error for the polynomial and cubic regressions
# ploy(2)
lm.fit2=lm(mpg~poly(horsepower,2) ,data=Auto,subset=train)
  mean((mpg-predict(lm.fit2,Auto))[-train]^2)
# ploy(3)
lm.fit3=lm(mpg~poly(horsepower ,3),data=Auto,subset=train) 
  mean((mpg-predict(lm.fit3,Auto))[-train]^2)
# error rates are 18.72 and 18.79
  
# If we choose a different training set instead, then we will obtain somewhat different errors on the validation set.
set.seed(2)
train=sample(392,196)
# linear
lm.fit=lm(mpg~horsepower,subset=train)
  mean((mpg-predict(lm.fit,Auto))[-train]^2)
# quadratic
lm.fit2=lm(mpg~poly(horsepower ,2),data=Auto,subset=train)
  mean((mpg-predict(lm.fit2,Auto))[-train]^2)
# cubic
lm.fit3=lm(mpg~poly(horsepower ,3),data=Auto,subset=train) 
  mean((mpg-predict(lm.fit3,Auto))[-train]^2)
# the validation set error rates for the models with linear, quadratic, and cubic terms are 
  # 25.73, 20.43, and 20.39
  
# a model that predicts mpg using a quadratic function of horsepower performs better than a model that involves only a linear function of horsepower
# there is little evidence in favor of a model that uses a cubic function of horsepower.



  # 5.3.2 Leave-One-Out Cross-Validation 
# The LOOCV estimate can be automatically computed for any generalized linear model using the glm() and cv.glm() functions.
# if we use glm() to fit a model without passing in the family argument, then it performs linear regression, just like the lm() function
glm.fit=glm(mpg~horsepower ,data=Auto) 
coef(glm.fit)

lm.fit=lm(mpg~horsepower ,data=Auto)
coef(lm.fit)
# yield identical linear regression models

# perform linear regression using the glm() function rather than the lm() function because the latter can be used together with cv.glm()
# The cv.glm() function is part of the boot library
library(boot)
glm.fit=glm(mpg~horsepower ,data=Auto)
cv.err=cv.glm(Auto,glm.fit)
cv.err$delta
# The cv.glm() function produces a list with several components.
# The two numbers in the delta vector contain the cross-validation results
# cross-validation estimate for the test error is approximately 24.23

# To automate the process, we use the for() function to initiate a for loop which iteratively fits polynomial regressions for polynomials of order i = 1 to i = 5
# computes the associated cross-validation error, and stores it in the ith element of the vector cv.error
# begin by initializing the vector
cv.error=rep(0,5)
for (i in 1:5){
glm.fit=glm(mpg~poly(horsepower ,i),data=Auto)
cv.error[i]=cv.glm(Auto,glm.fit)$delta[1]
}
cv.error
# see a sharp drop in the estimated test MSE between the linear and quadratic fits, but then no clear improvement from using higher-order polynomials


  
  
  # 5.3.3 k-Fold Cross-Validation 
# The cv.glm() function can also be used to implement k-fold CV
# use k = 10, a common choice for k, on the Auto data set
# set a random seed and initialize a vector in which we will store the CV errors corresponding to the polynomial fits of orders one to ten
set.seed(17)
cv.error.10=rep(0,10)
for (i in 1:10){
glm.fit=glm(mpg~poly(horsepower ,i),data=Auto)
cv.error.10[i]=cv.glm(Auto,glm.fit,K=10)$delta[1]
}
cv.error.10
# the computation time is much shorter than that of LOOCV
# see little evidence that using cubic or higher-order polynomial terms leads to lower test error than simply using a quadratic fit
# first is the standard k-fold CV estimate
# second is a bias- corrected version  


  # 5.3.4 The Bootstrap 
    # Estimating the Accuracy of a Statistic of Interest
# One of the great advantages of the bootstrap approach is that it can be applied in almost all situations
# No complicated mathematical calculations are required
# First, we must create a function that computes the statistic of interest. 
# Second, we use the boot() function, which is part of the boot library, to perform the bootstrap by repeatedly sampling observations from the data set with replacement

# first create a function, alpha.fn(), which takes as input the (X,Y) data as well as a vector indicating which observations should be used to estimate α
# The function then outputs the estimate for α based on the selected observations
alpha.fn=function(data,index){
X=data$X[index]
Y=data$Y[index]
return((var(Y)-cov(X,Y))/(var(X)+var(Y)-2*cov(X,Y)))
}
# function returns, or outputs, an estimate for α based on applying to the observations indexed by the argument index.

# to estimate α using all 100 observations
alpha.fn(Portfolio ,1:100)

# uses the sample() function to randomly select 100 ob- servations from the range 1 to 100, with replacement
# equivalent to constructing a new bootstrap data set and recomputing αˆ based on the new data set
set.seed(1)
alpha.fn(Portfolio,sample(100,100,replace=T))
# can implement a bootstrap analysis by performing this command many times, recording all of the corresponding estimates for α, and computing the resulting standard deviation

# the boot() function automates this approach
# produce R = 1,000 bootstrap estimates for α
boot(Portfolio ,alpha.fn,R=1000)
# final output shows that using the original data, αˆ and the bootstrap estimate for SE(αˆ)

    # Estimating the Accuracy of a Linear Regression Model
# The bootstrap approach can be used to assess the variability of the coef- ficient estimates and predictions from a statistical learning method
# use the bootstrap approach in order to assess the variability of the estimates for β0 and β1, the intercept and slope terms for the linear regres- sion model that uses horsepower to predict mpg
# compare the estimates obtained using the bootstrap to those obtained using the formulas for SE(βˆ0) and SE(βˆ1)

# create a simple function, boot.fn(), which takes in the Auto data set as well as a set of indices for the observations,
# returns the intercept and slope estimates for the linear regression model.
# apply this function to the full set of 392 observations in order to compute the esti- mates of β0 and β1 on the entire data set using the usual linear regression coefficient estimate formulas
boot.fn=function(data,index)
  + return(coef(lm(mpg~horsepower ,data=data,subset=index))) 
boot.fn(Auto ,1:392)

# The boot.fn() function can also be used in order to create bootstrap esti- mates for the intercept and slope terms by randomly sampling from among the observations with replacement
set.seed(1)
boot.fn(Auto,sample(392,392,replace=T))
boot.fn(Auto,sample(392,392,replace=T))

# we use the boot() function to compute the standard errors of 1,000 bootstrap estimates for the intercept and slope terms
boot(Auto ,boot.fn ,1000)
# indicates that the bootstrap estimate for SE(βˆ0), and that the bootstrap estimate for SE(βˆ1)

# standard formulas can be used to compute the standard errors for the regression coefficients in a linear model
# can be obtained using the summary() function
summary(lm(mpg~horsepower ,data=Auto))$coef
# The standard error estimates for βˆ0 and βˆ1 obtained using the formulas
# these are somewhat different from the estimates obtained using the bootstrap

# compute the bootstrap standard error estimates and the stan- dard linear regression estimates that result from fitting the quadratic model to the data
# Since this model provides a good fit to the data, there is now a better correspondence between the bootstrap estimates and the standard estimates of SE(βˆ0), SE(βˆ1) and SE(βˆ2).
boot.fn=function(data,index)
  + coefficients(lm(mpg~horsepower+I(horsepower^2),data=data,
                    subset=index))
set.seed (1)
boot(Auto ,boot.fn ,1000)

summary(lm(mpg~horsepower+I(horsepower^2),data=Auto))$coef





# Chap 5 Applied Exercises
# Q5: holdout method
  # a. now estimate the test error of this logistic regression model using the validation set approach
    # logisitc regression to predict the probability of “default” using “income” and “balance” on the “Default” data set
library(ISLR)
attach(Default)
set.seed(1)
fit.glm <- glm(default ~ income + balance, data = Default, family = "binomial")
summary(fit.glm)
  # b. Using the validation set approach, estimate the test error of this model
# i. Split the sample set into a training set and a validation set.
train <- sample(dim(Default)[1], dim(Default)[1] / 2)

# ii. Fit a multiple logistic regression model using only the training observations. ?????
fit.glm <- glm(default ~ income + balance, data = Default, family = "binomial", subset = train)
summary(fit.glm)

# iii. Obtain a prediction of default status for each individual in the validation set by computing the posterior
  # probability of default for that individual, and classifying the individual to the “default” category if the
  # posterior probability is greater than 0.5.
probs <- predict(fit.glm, newdata = Default[-train, ], type = "response")
pred.glm <- rep("No", length(probs))
pred.glm[probs > 0.5] <- "Yes"

# iv. Compute the validation set error, which is the fraction of the observations in the validation set that are misclassified.
mean(pred.glm != Default[-train, ]$default)
# We have a 2.54%  test error rate with the validation set approach.

  # c. use three different splits of the observations into a training set and a validation set
# 1
train <- sample(dim(Default)[1], dim(Default)[1] / 2)
fit.glm <- glm(default ~ income + balance, data = Default, family = "binomial", subset = train)
probs <- predict(fit.glm, newdata = Default[-train, ], type = "response")
pred.glm <- rep("No", length(probs))
pred.glm[probs > 0.5] <- "Yes"
mean(pred.glm != Default[-train, ]$default)
# 2
train <- sample(dim(Default)[1], dim(Default)[1] / 2)
fit.glm <- glm(default ~ income + balance, data = Default, family = "binomial", subset = train)
probs <- predict(fit.glm, newdata = Default[-train, ], type = "response")
pred.glm <- rep("No", length(probs))
pred.glm[probs > 0.5] <- "Yes"
mean(pred.glm != Default[-train, ]$default)
# 3
train <- sample(dim(Default)[1], dim(Default)[1] / 2)
fit.glm <- glm(default ~ income + balance, data = Default, family = "binomial", subset = train)
probs <- predict(fit.glm, newdata = Default[-train, ], type = "response")
pred.glm <- rep("No", length(probs))
pred.glm[probs > 0.5] <- "Yes"
mean(pred.glm != Default[-train, ]$default)

# the validation estimate of the test error rate can be variable, depending on precisely which observations are included in the training set and which observations are included in the validation set
  
  # d. bootstrap
    # logistic regression model that predicts the probability of “default” using “income”, “balance”, and a dummy variable for “student”. Estimate the test error for this model using the validation set approach
train <- sample(dim(Default)[1], dim(Default)[1] / 2)
fit.glm <- glm(default ~ income + balance + student, data = Default, family = "binomial", subset = train)
probs <- predict(fit.glm, newdata = Default[-train, ], type = "response")
pred.glm <- rep("No", length(probs))
pred.glm[probs > 0.5] <- "Yes"
mean(pred.glm != Default[-train, ]$default)
# doesn’t seem that adding the “student” dummy variable leads to a reduction in the validation set estimate of the test error rate.



# Q7: LOOCV
  # a. Fit a logistic regression model that predicts “Direction” using “Lag1” and “Lag2”
set.seed(1)
attach(Weekly)
fit.glm <- glm(Direction ~ Lag1 + Lag2, data = Weekly, family = "binomial")
summary(fit.glm)

  # b. Fit a logistic regression model that predicts “Direction” using “Lag1” and “Lag2” using all but the first observation
fit.glm.1 <- glm(Direction ~ Lag1 + Lag2, data = Weekly[-1, ], family = "binomial")
summary(fit.glm.1)

  # c. predict the direction of the first observation. You can do this by predicting that the first observation will go up
predict.glm(fit.glm.1, Weekly[1, ], type = "response") > 0.5
# We may conclude that the prediction for the first observation is “Up”. This observation was not correctly classified as the true direction is “Down”
as.character(Weekly[1, "Direction"])

  # d.Write a loop from to , where is the number of observations in the data set
error <- rep(0, dim(Weekly)[1])
for (i in 1:dim(Weekly)[1]) {
  fit.glm <- glm(Direction ~ Lag1 + Lag2, data = Weekly[-i, ], family = "binomial")
  pred.up <- predict.glm(fit.glm, Weekly[i, ], type = "response") > 0.5
  true.up <- Weekly[i, ]$Direction == "Up"
  if (pred.up != true.up)
    error[i] <- 1
}
error

  # e.  Take the average of the numbers obtained in (d)iv in order to obtain the LOOCV estimate for the n test error
mean(error)
# The LOOCV estimate for the test error rate is 44.9954086%



# Q9:
  # a. Based on this data set, provide an estimate for the population mean of “medv”. Call this estimate 
library(MASS)
attach(Boston)
mu.hat <- mean(medv)
mu.hat

  # b. Provide an estimate of the standard error of ^μ Interpret this result
se.hat <- sd(medv) / sqrt(dim(Boston)[1])
se.hat

  # c. Now estimate the standard error of ^μ using the bootstrap
set.seed(1)
boot.fn <- function(data, index) {
  mu <- mean(data[index])
  return (mu)
}
boot(medv, boot.fn, 1000)
# The bootstrap estimated standard error of of 0.4119 is very close to the estimate found in (b) of 0.4089

  # d. Based on your bootstrap estimate from (c), provide a 95% confidence interval for the mean of “medv”
t.test(medv)
CI.mu.hat <- c(22.53 - 2 * 0.4119, 22.53 + 2 * 0.4119)
CI.mu.hat
# The bootstrap confidence interval is very close to the one provided by the t.test() function




