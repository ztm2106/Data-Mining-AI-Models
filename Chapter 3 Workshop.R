# Chapter 3 Conceptual

# Q2 ISLR. Carefully explain the differences between the KNN classifier and KNN regression methods
    # KNN Classifier (K-nearest neighbors) = attempt to estimate the conditional distribution of Y given X, and then classify a given observation to the class with highest estimated probability
         # Given a positive integer K and a test observation x0, the KNN classifier first identifies the K points in the training data that are closest to x0, represented by N0. It then estimates the conditional probability 
         # for class j as the fraction of points in N0 whose response values equal j
    # KNN regression is a non-parametric method that, in an intuitive manner, approximates the association between independent variables and the continuous outcome by averaging the observations in the same neighbourhood
# KNN regression tries to predict the value of the output variable by using a local average. KNN classification attempts to predict the class to which the output variable belong by computing the local probability



# 3.6 Lab: Linear Regression

    # 3.6.1 Libraries
# The library() function is used to load libraries, or groups of functions and data sets that are not included in the base R distribution. Basic functions that perform least squares linear regression and other simple analyses come standard with the base distribution, but more exotic functions require ad- ditional libraries.
install.packages("MASS")
install.packages("ISLR")

library(MASS)
library(ISLR)

    # 3.6.2 Simple Linear Regression
fix(Boston)
names(Boston)

# type lm.fit, some basic information about the model is output. For more detailed information, we use summary(lm.fit). This gives us p- values and standard errors for the coefficients, as well as the R2 statistic and F-statistic for the model
lm.fit=lm(medv~lstat ,data=Boston)
attach(Boston)
lm.fit=lm(medv~lstat)

lm.fit
summary(lm.fit)

# use the names() function in order to find out what other pieces of information are stored in lm.fit. Although we can extract these quan- tities by name—e.g. lm.fit$coefficients—it is safer to use the extractor functions like coef() to access them
names(lm.fit)
coef(lm.fit)
# confidence interval for the coefficient estimates, we can use the confint() command
confint(lm.fit)

# The predict() function can be used to produce confidence intervals and prediction intervals for the prediction of medv for a given value of lstat
predict(lm.fit,data.frame(lstat=(c(5,10,15))), interval ="confidence")
predict(lm.fit,data.frame(lstat=(c(5,10,15))), interval ="prediction")

# plot medv and lstat along with the least squares regression line using the plot() and abline()
# There is some evidence for non-linearity in the relationship between lstat and medv.
# The abline() function can be used to draw any line, not just the least squares regression line. To draw a line with intercept a and slope b, we type abline(a,b).
dev.new()
plot(lstat ,medv)
abline(lm.fit,)

# The lwd=3 command causes the width of the regression line to be increased by a factor of 3; this works for the plot() and lines() functions also. We can also use the pch option to create different plotting symbols.

abline(lm.fit,lwd=3)
abline(lm.fit,lwd=3,col="red")
plot(lstat,medv,col="red")
plot(lstat,medv,pch=20)
plot(lstat,medv,pch="+")
plot(1:20,1:20,pch=1:20)

# divides the plotting region into a 2 × 2 grid of panels
par(mfrow=c(2,2))
plot(lm.fit)

# we can compute the residuals from a linear regression fit using the residuals() function. The function rstudent() will return the studentized residuals,
dev.new()
par(mfrow=c(1,2))
plot(predict(lm.fit), residuals(lm.fit))
plot(predict(lm.fit), rstudent(lm.fit))

# Leverage statistics can be computed for any number of predictors using the hatvalues()
dev.new()
plot(hatvalues(lm.fit))
which.max(hatvalues(lm.fit))

    #3.6.3 Multiple Linear Regression
# In order to fit a multiple linear regression model using least squares, we again use the lm() function. The syntax lm(y∼x1+x2+x3) is used to fit a model with three predictors, x1, x2, and x3. The summary() function now outputs the regression coefficients for all the predictors.
lm.fit=lm(medv~lstat+age,data=Boston)
summary(lm.fit)

# all of these in order to perform a regression using all of the predictors
lm.fit=lm(medv~.,data=Boston)
summary(lm.fit)

# The vif() function, part of the car package, can be used to compute variance inflation factors
  # the variance inflation factor is the ratio of the variance of estimating some parameter in a model that includes multiple other terms by the variance of a model constructed using only one term. It quantifies the severity of multicollinearity in an ordinary least squares regression analysis.
install.packages("car")
library(car)
vif(lm.fit)

# we may wish to run a regression excluding this predictor. The following syntax results in a regression using all predictors except age
lm.fit1=lm(medv~.-age,data=Boston)
summary(lm.fit1)
  #same thing just using update() instead
lm.fit1=update(lm.fit, ~.-age)
summary(lm.fit1)


    # 3.6.4 Interaction Terms
# The syntax lstat:black tells R to include an interaction term between lstat and black. The syntax lstat*age simultaneously includes lstat, age, and the interaction term lstat×age as predictors; it is a shorthand for lstat+age+lstat:age
summary(lm(medv~lstat*age,data=Boston))

      #3.6.5 Non-linear Transformations of the Predictors
# The lm() function can also accommodate non-linear transformations of the predictors. For instance, given a predictor X, we can create a predictor X2 using I(X^2). The function I() is needed since the ^ has a special meaning in a formula
lm.fit2=lm(medv~lstat+I(lstat^2))
summary(lm.fit2)


# We use the anova() function to further quantify the extent to which the quadratic fit is superior to the linear fit
lm.fit=lm(medv~lstat)
anova(lm.fit ,lm.fit2)

# evidence for non-linearity in the relationship
dev.new()
par(mfrow=c(2,2))
plot(lm.fit2)

# A better approach involves using the poly() function to create the polynomial within lm()
lm.fit5=lm(medv~poly(lstat ,5))
summary(lm.fit5)

# log transformation
summary(lm(medv~log(rm),data=Boston))

      #3.6.6 Qualitative Predictors
# The Carseats data includes qualitative predictors 
fix(Carseats)
names(Carseats)

lm.fit=lm(Sales~.+Income:Advertising+Price:Age,data=Carseats)
summary(lm.fit)

# The contrasts() function returns the coding that R uses for the dummy variables.
attach(Carseats)
contrasts(ShelveLoc)


      # 3.6.7 Writing Functions
# The { symbol informs R that multiple commands are about to be input. Hitting Enter after typing { will cause R to print the + symbol. We can then input as many commands as we wish, hitting Enter after each one. Finally the } symbol informs R that no further commands will be entered.
LoadLibraries=function (){
  library(ISLR)
  library(MASS)
  print("The libraries have been loaded.")
}

LoadLibraries
LoadLibraries()

# Applied
# Q8
# (a)
library(ISLR)
data(Auto)
fit <- lm(mpg ~ horsepower, data = Auto)
summary(fit)

# i. Is there a relationship between the predictor and the response?
    #by testing the hypothesis . The p-value corresponding to the F-statistic is7.03198910^{-81}, this indicates a clear evidence of a relationship between “mpg” and “horsepower

# ii. How strong is the relationship between the predictor and the response?
    #yes there is a storng relationship between the predictor and response by looking at the R-squared value. About 60% of mpg can be explained by horsepower
summary(fit)$sigma
summary(fit)$sigma / mean(Auto$mpg)
# iii. Is the relationship between the predictor and the response positive or negative?
    # negative rrelationship when looking at the slope which is estimated to be  -0.157845.he more horsepower an automobile has the linearregression indicates the less mpg fuel efficiency the automobile will have

# iv. What is the predicted mpg associated with a horsepower of 98? What are the associated 95 % confidence and prediction intervals?

predict(fit, data.frame(horsepower = 98), interval = "confidence")
predict(fit, data.frame(horsepower = 98), interval = "prediction")

# (b)
dev.new()
plot(Auto$horsepower, Auto$mpg, main = "Scatterplot of mpg vs. horsepower", xlab = "horsepower", ylab ="mpg", col = "blue")
abline(fit, col = "red")

# (c)
dev.new()
par(mfrow = c(2, 2))
plot(fit)
    #The plot of residuals versus fitted values indicates the presence of non linearity in the data. The plot of standardized residuals versusleverage indicates the presence of a few outliers (higher than 2 or lower than -2) and a few high leverage points.


# Q9
# (a)
dev.new()
pairs(Auto)

# (b)
names(Auto)
cor(Auto[1:8])

# (c)
fit2 <- lm(mpg ~ . -name, data = Auto)
summary(fit2)

# i. Is there a relationship between the predictors and the re- sponse?
  # yes there is a relationship between the predictors and mpg

# ii. Which predictors appear to have a statistically significant relationship to the response?
  #by looking at the pvalues associated cylinders, horsepower, and accel. do not have statistically significant relationships to mpg like other variables. their p values are over 0.05

# iii. What does the coefficient for the year variable suggest?
  #the coefficient for the year variable suggest The coefficient ot the “year” variable suggests that the average effect of an increase of 1 year is an increase of 0.7507727 in “mpg”(all other predictors remaining constant). In other words, cars become more fuel efficient every year by almost 1 mpg / year 

# (d)
dev.new()
par(mfrow=c(2,2))
plot(fit2)
  
# the residual v fitted shows there a non linear relationship.
  #The plot of standardizedresiduals versus leverage indicates the presence of a few outliers (higher than 2 or lower than -2) and one high leverage point (point14)

# (e)
fit3 <- lm(mpg ~ cylinders * displacement+displacement * weight, data = Auto[, 1:8])
summary(fit3)
  # by looking at the pvalues displacement:weight is significant but cylinders:displacement is not

# (f)
dev.new()
par(mfrow = c(2, 2))
plot(log(Auto$horsepower), Auto$mpg)
plot(sqrt(Auto$horsepower), Auto$mpg)
plot((Auto$horsepower)^2, Auto$mpg)

  #there are similar however the log transformation give the most linear look to the plot limiting it to horsepower 



#Q14
# (a)
set.seed(1)
x1=runif(100)
x2=0.5*x1+rnorm(100)/10
y=2+2*x1+0.3*x2+rnorm(100)

#regression coeff.
#b0 = 2
#b1 = 2 
#b2 = 0.3

# (b)
cor(x1,x2)
dev.new()
plot(x1,x2)

# (c)
fit13 <- lm(y ~ x1 + x2)
summary(fit13)

#b0 = 2.1305 this is the only one close to the estimate B0
#b1 = 1.4396
#b2 = 1.0097
#looking at p values, we can reject b1 but not b2 since the p value is greater than 0.05

# (d)
fit14 <- lm(y ~ x1)
summary(fit14)
#b1 = 1.9759 is now close to estimated b1
#we can reject the null hypo of b1 

# (e)
fit15 <- lm(y ~ x2)
summary(fit15)
#b1 = 2.8996 is not close to estimated b1
#we can reject the null hypo of b1 

# (g)
x1=c(x1, 0.1)
x2=c(x2, 0.8)
y=c(y,6)

fit16 <- lm(y ~ x1 + x2)
fit17 <- lm(y ~ x1)
fit18 <- lm(y ~ x2)
summary(fit16)
summary(fit17)
summary(fit18)
dev.new()
par(mfrow = c(2, 2))
plot(fit16)
plot(fit17)
plot(fit18)

# the last point is a high-leverage point. In the model with “x1” as sole predictor, the last point is anoutlier. In the model with “x2” as sole predictor, the last point is a high leverage point







