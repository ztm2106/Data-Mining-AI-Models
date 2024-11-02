# -------- Lab: Logistic Regression, LDA, QDA, and KNN -----------
  #4.6.1 The Stock Market Data
library(ISLR)
names(Smarket)
dim(Smarket)
summary(Smarket)
pairs(Smarket)

#cor() function produces a matrix that contains all of the pairwise correlations among the predictors in a data set.
cor(Smarket[,-9])

attach(Smarket) 
dev.new()
plot(Volume)

  #4.6.2 Logistic Regression
#The glm() function fits generalized linear models, a class of models that includes logistic regression. The syntax of the glm() function is similar to that of lm(), except that we must pass in the argument family=binomial in order to tell R to run a logistic regression
glm.fit=glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume , data=Smarket ,family=binomial)
summary(glm.fit)

#use the coef() function in order to access just the coefficients for this fitted model. We can also use the summary() function to access particular aspects of the fitted model, such as the p-values for the coefficients.
coef(glm.fit)
summary(glm.fit)$coef
summary(glm.fit)$coef[,4]

#The predict() function can be used to predict the probability that the market will go up, given values of the predictors. The type="response" option tells R to output probabilities of the form P(Y = 1|X), as opposed to other information such as the logit.
#We know that these values correspond to the probability of the market going up, rather than down, because the contrasts() function indicates that R has created a dummy variable with a 1 for Up
glm.probs=predict(glm.fit,type="response")
glm.probs[1:10]
contrasts(Direction )

#two commands create a vector of class predictions based on whether the predicted probability of a market increase is greater than or less than 0.5.
#convert these predicted probabilities into class labels, Up or Down
#The first command creates a vector of 1,250 Down elements. The second line transforms to Up all of the elements for which the predicted probability of a market increase exceeds 0.5.
glm.pred=rep("Down",1250)
glm.pred[glm.probs >.5]="Up"

#the table() function can be used to produce a confusion matrix in order to determine how many observations were correctly or incorrectly classified.
#The diagonal elements of the confusion matrix indicate correct predictions, while the off-diagonals represent incorrect predictions.
#The mean() function can be used to compute the fraction of days for which the prediction was correct.
table(glm.pred,Direction)
(507+145) /1250
mean(glm.pred==Direction )

#create a vector corresponding to the observations from 2001 through 2004. We will then use this vector to create a held out data set of observations from 2005.
train=(Year <2005)
Smarket.2005= Smarket [! train ,]
dim(Smarket.2005)
Direction.2005=Direction[!train]

# object train is a vector of 1,250 elements, corresponding to the ob- servations in our data set.
# The elements of the vector that correspond to observations that occurred before 2005 are set to TRUE, whereas those that correspond to observations in 2005 are set to FALSE. The object train is a Boolean vector, since its elements are TRUE and FALSE.
# Boolean vectors can be used to obtain a subset of the rows or columns of a matrix. For instance, the command Smarket[train,] would pick out a submatrix of the stock market data set, corresponding only to the dates before 2005
# The ! symbol can be used to reverse all of the elements of a Boolean vector. That is, !train is a vector similar to train, except that the elements that are TRUE in train get swapped to FALSE in !train, and the elements that are FALSE in train get swapped to TRUE in !train
# Smarket[!train,] yields a submatrix of the stock market data containing only the observations for which train is FALSE—that is, the observations with dates in 2005
# now fit a logistic regression model using only the subset of the obser- vations that correspond to dates before 2005, using the subset argument
glm.fit=glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume , data=Smarket ,family=binomial,subset=train)
glm.probs=predict(glm.fit,Smarket.2005,type="response")

# training was performed using only the dates before 2005, and testing was performed using only the dates in 2005. 
# Finally, we com- pute the predictions for 2005 and compare them to the actual movements of the market over that time period.
glm.pred=rep("Down",252)
glm.pred[glm.probs >.5]="Up"
table(glm.pred,Direction.2005)
mean(glm.pred==Direction.2005)
# The != notation means not equal to, and so the last command computes the test set error rate.
mean(glm.pred!=Direction.2005)

# we have refit the logistic regression using just Lag1 and Lag2, which seemed to have the highest predictive power in the original logistic regression model.
glm.fit=glm(Direction~Lag1+Lag2,data=Smarket ,family=binomial, subset=train)
glm.probs=predict(glm.fit,Smarket.2005,type="response")
glm.pred=rep("Down",252)
glm.pred[glm.probs >.5]="Up"
table(glm.pred,Direction.2005)
mean(glm.pred==Direction.2005)
106/(106+76)

# In particular, we want to predict Direction on a day when Lag1 and Lag2 equal 1.2 and 1.1, respectively, and on a day when they equal 1.5 and −0.8. We do this using the predict() function.
predict(glm.fit,newdata=data.frame(Lag1=c(1.2,1.5), Lag2=c(1.1,-0.8)),type="response")


  # 4.6.3 Linear Discriminant Analysis
# we fit a LDA model using the lda() function, which is part of the MASS library.
library(MASS)
lda.fit=lda(Direction~Lag1+Lag2,data=Smarket ,subset=train)
# The coefficients of linear discriminants output provides the linear combination of Lag1 and Lag2 that are used to form the LDA decision rule.
lda.fit
# The plot() function produces plots of the linear discriminants, obtained by computing −0.642 × Lag1 − 0.514 × Lag2 for each of the training observations.
dev.new()
plot(lda.fit)

# The predict() function returns a list with three elements.
# The first ele- ment, class, contains LDA’s predictions about the movement of the market.
# The second element, posterior, is a matrix whose kth column contains the posterior probability that the corresponding observation belongs to the kth class
# x contains the linear discriminants
lda.pred=predict(lda.fit, Smarket.2005)
names(lda.pred)

# the LDA and logistic regression predictions are almost identical
lda.class=lda.pred$class
table(lda.class ,Direction.2005)
mean(lda.class==Direction.2005)

# Applying a 50 % threshold to the posterior probabilities allows us to recre- ate the predictions contained in lda.pred$class.
sum(lda.pred$posterior[,1]>=.5)
sum(lda.pred$posterior[,1]<.5)

# the posterior probability output by the model corresponds to the probability that the market will decrease
lda.pred$posterior[1:20,1] 
lda.class[1:20]

#  suppose that we wish to predict a market decrease only if we are very certain that the market will indeed decrease on that day—say, if the posterior probability is at least 90 %
# No days in 2005 meet that threshold! In fact, the greatest posterior prob- ability of decrease in all of 2005 was 52.02 %
sum(lda.pred$posterior[,1]>.9)

  # 4.6.4 Quadratic Discriminant Analysis
# now fit a QDA model to the Smarket data. QDA is implemented in R using the qda() function, which is also part of the MASS library.
# The output contains the group means. But it does not contain the coef- ficients of the linear discriminants, because the QDA classifier involves a quadratic
qda.fit=qda(Direction~Lag1+Lag2,data=Smarket ,subset=train)
qda.fit

# the QDA predictions are accurate almost 60% of the time, even though the 2005 data was not used to fit the model
qda.class=predict(qda.fit,Smarket.2005)$class
table(qda.class ,Direction.2005)
mean(qda.class==Direction.2005)

# the quadratic form assumed by QDA may capture the true relationship more accurately than the linear forms assumed by LDA and logistic regression. However, we recommend evaluating this method’s performance on a larger test set before betting that this approach will consistently beat the market!

  # 4.6.5 K-Nearest Neighbors  
# will now perform KNN using the knn() function, which is part of the class library. 
# knn() forms predictions using a single command. The function requires four inputs.
# 1. A matrix containing the predictors associated with the training data, labeled train.X below
# 2. A matrix containing the predictors associated with the data for which we wish to make predictions, labeled test.X below.
# 3. A vector containing the class labels for the training observations, labeled train.Direction below.
# 4. A value for K, the number of nearest neighbors to be used by the classifier.

# cbind() function, short for column bind, to bind the Lag1 and Lag2 variables together into two matrices, one for the training set and the other for the test set.
library(class)
train.X=cbind(Lag1 ,Lag2)[train ,]
test.X=cbind(Lag1,Lag2)[!train,]
train.Direction=Direction [train]

# set a random seed before we apply knn() because if several observations are tied as nearest neighbors, then R will randomly break the tie. Therefore, a seed must be set in order to ensure reproducibil- ity of results
set.seed(1)
knn.pred=knn(train.X,test.X,train.Direction ,k=1)
table(knn.pred,Direction.2005)
(83+43) /252

# we repeat the analysis using K = 3 to check for better results
knn.pred=knn(train.X,test.X,train.Direction ,k=3)
table(knn.pred,Direction.2005)
mean(knn.pred==Direction.2005)

# But increasing K further turns out to provide no further improvements. It appears that for this data, QDA provides the best results of the methods that we have examined so far

  # 4.6.6 An Application to Caravan Insurance Data
# we will apply the KNN approach to the Caravan data set, which is part of the ISLR library.
dim(Caravan)
attach(Caravan)
summary(Purchase)
348/5822

# good way to handle this problem is to standardize the data so that all variables are given a mean of zero and a standard deviation of one.
# The scale() function does just this. In standardizing the data, we exclude column 86, because that is the qualitative Purchase variable.
standardized.X=scale(Caravan [,-86])
var(Caravan[ ,1])
var(Caravan[ ,2])
var(standardized.X[,1])
var(standardized.X[,2])
# Now every column of standardized.X has a standard deviation of one and a mean of zero.

# split the observations into a test set, containing the first 1,000 observations, and a training set, containing the remaining observations.
# fit a KNN model on the training data using K = 1, and evaluate its performance on the test data.

test =1:1000
train.X=standardized.X[-test ,]
test.X=standardized.X[test ,]
train.Y=Purchase [-test]
test.Y=Purchase [test]
set.seed (1)
knn.pred=knn(train.X,test.X,train.Y,k=1)
mean(test.Y!=knn.pred)
mean(test.Y!="No")
# Typing standardized.X[test,] yields the submatrix of the data containing the ob- servations whose indices range from 1 to 1, 000
# typing standardized.X[-test,] yields the submatrix containing the observations whose indices do not range from 1 to 1, 000

# double the rate that one would obtain from random guessing
table(knn.pred,test.Y)
9/(68+9)

# Using K = 3, the success rate increases to 19 %, and with K = 5 the rate is 26.7 %.
# KNN is finding some real patterns in a difficult data set!
knn.pred=knn(train.X,test.X,train.Y,k=3) 
table(knn.pred,test.Y)
5/26

knn.pred=knn(train.X,test.X,train.Y,k=5)
table(knn.pred,test.Y)
4/15

# As a comparison, we can also fit a logistic regression model to the data. If we use 0.5 as the predicted probability cut-off for the classifier
glm.fit=glm(Purchase~.,data=Caravan ,family=binomial, subset=-test)
glm.probs=predict(glm.fit,Caravan[test,],type="response")
glm.pred=rep("No",1000)
glm.pred[glm.probs >.5]="Yes"
table(glm.pred,test.Y)

# we instead predict a purchase any time the predicted probability of purchase exceeds 0.25, we get much better results
glm.pred=rep("No",1000)
glm.pred[glm.probs >.25]=" Yes"
table(glm.pred,test.Y)
11/(22+11)

                # Applied 11
# a
attach(Auto)
mpg01 <- rep(0, length(mpg))
mpg01[mpg > median(mpg)] <- 1
Auto <- data.frame(Auto, mpg01)

# b
cor(Auto[, -9])
dev.new()
pairs(Auto)
boxplot(cylinders ~ mpg01, data = Auto, main = "Cylinders vs mpg01")
boxplot(displacement ~ mpg01, data = Auto, main = "Displacement vs mpg01")
boxplot(horsepower ~ mpg01, data = Auto, main = "Horsepower vs mpg01")
boxplot(weight ~ mpg01, data = Auto, main = "Weight vs mpg01")
boxplot(acceleration ~ mpg01, data = Auto, main = "Acceleration vs mpg01")
boxplot(year ~ mpg01, data = Auto, main = "Year vs mpg01")

# We may conclude that there exists some association between “mpg01” and “cylinders”, “weight”,“displacement” and “horsepower”

# c 
# Split the data into a training set and a test set
train <- (year %% 2 == 0)
Auto.train <- Auto[train, ]
Auto.test <- Auto[!train, ]
mpg01.test <- mpg01[!train]

# d
# Perform LDA on the training data in order to predict “mpg01” using the variables that seemed mostassociated with “mpg01” in (b). 
# What is the test error of the model obtained ?
fit.lda <- lda(mpg01 ~ cylinders + weight + displacement + horsepower, data = Auto, subset = train)
fit.lda

pred.lda <- predict(fit.lda, Auto.test)
table(pred.lda$class, mpg01.test)
mean(pred.lda$class != mpg01.test)
# We may conclude that we have a test error rate of 12.6373626%

# e
# Perform QDA on the training data in order to predict “mpg01” using the variables that seemed mostassociated with “mpg01” in (b). What is the test error of the model obtained ?
fit.qda <- qda(mpg01 ~ cylinders + weight + displacement + horsepower, data = Auto, subset = train)
fit.qda

pred.qda <- predict(fit.qda, Auto.test)
table(pred.qda$class, mpg01.test)
mean(pred.qda$class != mpg01.test)
# We may conclude that we have a test error rate of 13.1868132%

# f
#  Perform logistic regression on the training data in order to predict “mpg01” using the variables that seemed most associated with “mpg01” in (b). What is the test error of the model obtained ?
fit.glm <- glm(mpg01 ~ cylinders + weight + displacement + horsepower, data = Auto, family = binomial, subset = train)
summary(fit.glm)

probs <- predict(fit.glm, Auto.test, type = "response")
pred.glm <- rep(0, length(probs))
pred.glm[probs > 0.5] <- 1
table(pred.glm, mpg01.test)
mean(pred.glm != mpg01.test)
# We may conclude that we have a test error rate of 12.0879121%

# g
# Perform KNN on the training data, with several values of  , in order to predict “mpg01” using thevariables that seemed most associated with “mpg01” in (b). What test errors do you obtain ? Whichvalue of   seems to perform the best on this data set ?
train.X <- cbind(cylinders, weight, displacement, horsepower)[train, ]
test.X <- cbind(cylinders, weight, displacement, horsepower)[!train, ]
train.mpg01 <- mpg01[train]
set.seed(1)
pred.knn <- knn(train.X, test.X, train.mpg01, k = 1)
table(pred.knn, mpg01.test)
mean(pred.knn != mpg01.test)
# We may conclude that we have a test error rate of 15.3846154% for K=1

pred.knn <- knn(train.X, test.X, train.mpg01, k = 10)
table(pred.knn, mpg01.test)
mean(pred.knn != mpg01.test)
# We may conclude that we have a test error rate of 16.4835165% for K=10.

pred.knn <- knn(train.X, test.X, train.mpg01, k = 100)
table(pred.knn, mpg01.test)
mean(pred.knn != mpg01.test)
# We may conclude that we have a test error rate of 14.2857143% for K=100 . So, a K value of 100 seems to perform the best


