# 6.5 Lab 1: Subset Selection Methods
      # 6.5.1 Best Subset Selection
# The is.na() function can be used to identify the missing observations
# It returns a vector of the same length as the input vector, with a TRUE for any elements that are missing, and a FALSE for non-missing elements.

# The sum() function can then be used to count all of the missing elements
library(ISLR) 
fix(Hitters)
names(Hitters)
dim(Hitters)
sum(is.na(Hitters$Salary))

# The na.omit() function removes all of the rows that have missing values in any variable
Hitters=na.omit(Hitters) 
dim(Hitters)
sum(is.na(Hitters))

# The regsubsets() function (part of the leaps library) performs best sub- set selection by identifying the best model that contains a given number of predictors, where best is quantified using RSS
# The syntax is the same as for lm(). 
# The summary() command outputs the best set of variables for each model size.
library(leaps)
regfit.full=regsubsets(Salary~.,Hitters)
summary(regfit.full)
# An asterisk indicates that a given variable is included in the corresponding model
# this output indicates that the best two-variable model contains only Hits and CRBI
# By default, regsubsets() only reports results up to the best eight-variable model.

# the nvmax option can be used in order to return as many variables as are desired. Here we fit up to a 19-variable model.
regfit.full=regsubsets(Salary~.,data=Hitters ,nvmax=19)
reg.summary=summary(regfit.full)

# The summary() function also returns R2, RSS, adjusted R2, Cp, and BIC. We can examine these to try to select the best overall model.
names(reg.summary)

# we see that the R2 statistic increases from 32 %, when only one variable is included in the model, to almost 55%, when all variables are included. As expected, the R2 statistic increases monotonically as more variables are included
reg.summary$rsq

# Plotting RSS, adjusted R2, Cp, and BIC for all of the models at once will help us decide which model to select. 
# Note the type="l" option tells R to connect the plotted points with lines.
par(mfrow=c(2,2))
plot(reg.summary$rss ,xlab="Number of Variables ",ylab="RSS",type="l")
plot(reg.summary$adjr2 ,xlab="Number of Variables ",ylab="Adjusted RSq",type="l")

# The points() command works like the plot() command, except that it puts points on a plot that has already been created, instead of creating a new plot.
# The which.max() function can be used to identify the location of the maximum point of a vector. We will now plot a red dot to indicate the model with the largest adjusted R2 statistic
which.max(reg.summary$adjr2)
points(11,reg.summary$adjr2[11],col="red",cex=2,pch=20) ???????

# we can plot the Cp and BIC statistics, and indicate the models with the smallest statistic using which.min()
plot(reg.summary$cp ,xlab="Number of Variables",ylab="Cp", type='1')??????
which.min(reg.summary$cp)
points(10,reg.summary$cp[10],col="red",cex=2,pch=20)

which.min(reg.summary$bic)
plot(reg.summary$bic ,xlab="Number of Variables",ylab="BIC",type=’1’)?????
points(6,reg.summary$bic[6],col="red",cex=2,pch=20)

# The regsubsets() function has a built-in plot() command which can be used to display the selected variables for the best model with a given number of predictors, ranked according to the BIC, Cp, adjusted R2, or AIC.
plot(regfit.full,scale="r2")
plot(regfit.full,scale="adjr2")
plot(regfit.full,scale="Cp")
plot(regfit.full,scale="bic")
# The top row of each plot contains a black square for each variable selected according to the optimal model associated with that statistic.
# we see that several models share a BIC close to −150. However, the model with the lowest BIC is the six-variable model that contains only AtBat, Hits, Walks, CRBI, DivisionW, and PutOuts.

# use the coef() function to see the coefficient estimates associated with this model
coef(regfit.full ,6)


      # 6.5.2 Forward and Backward Stepwise Selection
# use the regsubsets() function to perform forward stepwise or backward stepwise selection, using the argument method="forward" or method="backward"
regfit.fwd=regsubsets(Salary~.,data=Hitters ,nvmax=19, method ="forward")
summary(regfit.fwd)
regfit.bwd=regsubsets(Salary~.,data=Hitters ,nvmax=19,method ="backward")
summary(regfit.bwd)
#  we see that using forward stepwise selection, the best one- variable model contains only CRBI, and the best two-variable model ad- ditionally includes Hits
# the best one-variable through six- variable models are each identical for best subset and forward selection. 
# However, the best seven-variable models identified by forward stepwise se- lection, backward stepwise selection, and best subset selection are different.

coef(regfit.full ,7)
coef(regfit.fwd ,7)
coef(regfit.bwd ,7)

      # 6.5.3 Choosing Among Models Using the Validation Set Approach and Cross-Validation
# In order for these approaches to yield accurate estimates of the test error, we must use only the training observations to perform all aspects of model-fitting—including variable selection.
# the determination of which model of a given size is best must be made using only the training observations.

# In order to use the validation set approach, we begin by splitting the observations into a training set and a test set. 
# We do this by creating a random vector, train, of elements equal to TRUE if the corresponding observation is in the training set, and FALSE otherwise.
# The vector test has a TRUE if the observation is in the test set, and a FALSE otherwise.
set.seed (1)
train=sample(c(TRUE,FALSE), nrow(Hitters),rep=TRUE)
test=(!train )

# we apply regsubsets() to the training set in order to perform best subset selection.
regfit.best=regsubsets(Salary~.,data=Hitters[train,], nvmax=19)
# we subset the Hitters data frame directly in the call in or- der to access only the training subset of the data, using the expression Hitters[train,]

# now compute the validation set error for the best model of each model size. We first make a model matrix from the test data.
test.mat=model.matrix(Salary~.,data=Hitters[test,])
# The model.matrix() function is used in many regression packages for build- ing an “X” matrix from data.

# we run a loop, and for each size i, we extract the coefficients from regfit.best for the best model of that size, multiply them into the appropriate columns of the test model matrix to form the predictions, and compute the test MSE
val.errors=rep(NA,19)
for(i in 1:19){
coefi=coef(regfit.best,id=i)
pred=test.mat[,names(coefi)]%*%coefi
val.errors[i]=mean((Hitters$Salary[test]-pred)^2) 
}

# find that the best model is the one that contains ten variables ??????
val.errors 
which.min(val.errors)
coef(regfit.best ,10)

# we will be using this function again, we can capture our steps above and write our own predict method.
predict.regsubsets =function (object ,newdata ,id ,...){
form=as.formula(object$call [[2]])
mat=model.matrix(form,newdata)
coefi=coef(object ,id=id)
xvars=names(coefi)
mat[,xvars]%*%coefi 
}
# The only complex part is how we extracted the formula used in the call to regsubsets(). We demonstrate how we use this function below, when we do cross-validation.

# we perform best subset selection on the full data set, and select the best ten-variable model.
# It is important that we make use of the full data set in order to obtain more accurate coefficient estimates.
# Note that we perform best subset selection on the full data set and select the best ten- variable model, rather than simply using the variables that were obtained from the training set, because the best ten-variable model on the full data set may differ from the corresponding model on the training set.
regfit.best=regsubsets(Salary~.,data=Hitters ,nvmax=19)
coef(regfit.best ,10)
# we see that the best ten-variable model on the full data set has a different set of variables than the best ten-variable model on the training set.

# now try to choose among the models of different sizes using cross- validation.
# we must perform best subset selection within each of the k training sets
# we create a vector that allocates each observation to one of k = 10 folds, and we create a matrix in which we will store the results.
k=10
set.seed(1)
folds=sample(1:k,nrow(Hitters),replace=TRUE)
cv.errors=matrix(NA,k,19, dimnames=list(NULL, paste(1:19))
                 
# we write a for loop that performs cross-validation. In the jth fold, the elements of folds that equal j are in the test set, and the remainder are in the training set.
# make our predictions for each model size (using our new predict() method), compute the test errors on the appropriate subset, and store them in the appropriate slot in the matrix cv.errors
#??????
for(j in 1:k){
  best.fit=regsubsets(Salary~.,data=Hitters[folds!=j,],nvmax=19)
  for(i in 1:19){
    pred=predict(best.fit,Hitters[folds==j,],id=i)
    cv.errors[j,i]=mean( (Hitters$Salary[folds==j]-pred)^2)
  }
}
# This has given us a 10×19 matrix, of which the (i, j)th element corresponds to the test MSE for the ith cross-validation fold for the best j-variable model. 

# use the apply() function to average over the columns of this matrix in order to obtain a vector for which the jth element is the cross- validation error for the j-variable model.
# ?????
mean.cv.errors=apply(cv.errors ,2,mean) 
mean.cv.errors

par(mfrow=c(1,1))
plot(mean.cv.errors ,type=’b’)
# We see that cross-validation selects an 11-variable model.

# now perform best subset selection on the full data set in order to obtain the 11-variable model.
reg.best=regsubsets (Salary~.,data=Hitters , nvmax=19)
coef(reg.best ,11)



      # 6.6 Lab 2: Ridge Regression and the Lasso
# use the glmnet package in order to perform ridge regression and the lasso.
# The main function in this package is glmnet(), which can be used to fit ridge regression models, lasso models, and more.
# we must pass in an x matrix as well as a y vector, and we do not use the y ∼ x syntax.

# We will now perform ridge regression and the lasso in order to predict Salary on the Hitters data. Before proceeding ensure that the missing values have been removed from the data, 6.5
x=model.matrix(Salary~.,Hitters)[,-1] 
y=Hitters$Salary
# The model.matrix() function is particularly useful for creating x; not only does it produce a matrix corresponding to the 19 predictors but it also automatically transforms any qualitative variables into dummy variables.
# The latter property is important because glmnet() can only take numerical, quantitative inputs.


  # 6.6.1 Ridge Regression
# The glmnet() function has an alpha argument that determines what type of model is fit
# If alpha=0 then a ridge regression model is fit, and if alpha=1 then a lasso model is fit. 
# first fit a ridge regression mode
library(glmnet)
grid=10^seq(10,-2,length=100)
ridge.mod=glmnet(x,y,alpha=0,lambda=grid)
# By default the glmnet() function performs ridge regression for an automati- cally selected range of λ values.
# However, here we have chosen to implement the function over a grid of values ranging from λ = 1010 to λ = 10−2, es- sentially covering the full range of scenarios from the null model containing only the intercept, to the least squares fit.

# As we will see, we can also com- pute model fits for a particular value of λ that is not one of the original grid values. Note that by default, the glmnet() function standardizes the variables so that they are on the same scale. To turn off this default setting, use the argument standardize=FALSE.

# each value of λ is a vector of ridge regression coefficients, stored in a matrix that can be accessed by coef(). 
dim(coef(ridge.mod))
# In this case, it is a 20×100 matrix, with 20 rows (one for each predictor, plus an intercept) and 100 columns (one for each value of λ).

# We expect the coefficient estimates to be much smaller, in terms of l2 norm, when a large value of λ is used, as compared to when a small value of λ is used.
ridge.mod$lambda [50]
# These are the coefficients when λ = 11,498, along with their l2 norm
coef(ridge.mod)[,50]
sqrt(sum(coef(ridge.mod)[-1,50]^2))

# In contrast, here are the coefficients when λ = 705, along with their l2 norm. Note the much larger l2 norm of the coefficients associated with this smaller value of λ.
ridge.mod$lambda [60]
coef(ridge.mod)[,60]
sqrt(sum(coef(ridge.mod)[-1,60]^2))

# We can use the predict() function for a number of purposes. 
# For instance, we can obtain the ridge regression coefficients for a new value of λ, say 50
#?????
predict(ridge.mod,s=50,type="coefficients")[1:20,]

# now split the samples into a training set and a test set in order to estimate the test error of ridge regression and the lasso.
#  first is to produce a random vector of TRUE, FALSE elements and select the observations corresponding to TRUE for the training data.
# second is to randomly choose a subset of numbers between 1 and n; these can then be used as the indices for the training observations.
set.seed(1)
train=sample(1:nrow(x), nrow(x)/2)
test=(-train)
y.test=y[test]

# we fit a ridge regression model on the training set, and evaluate its MSE on the test set, using λ = 4.
# we get predictions for a test set, by replacing type="coefficients" with the newx argument
#?????
ridge.mod=glmnet(x[train,],y[train],alpha=0,lambda=grid, thresh =1e-12)
ridge.pred=predict(ridge.mod,s=4,newx=x[test,]) 
mean((ridge.pred-y.test)^2)
# The test MSE

# we could compute the test set MSE like this:
mean((mean(y[train])-y.test)^2)

# could also get the same result by fitting a ridge regression model with a very large value of λ. Note that 1e10 means 10^10
ridge.pred=predict(ridge.mod,s=1e10,newx=x[test,]) 
mean((ridge.pred-y.test)^2)

# So fitting a ridge regression model with λ = 4 leads to a much lower test MSE than fitting a model with just an intercept. 

# now check whether there is any benefit to performing ridge regression with λ = 4 instead of just performing least squares regression. Recall that least squares is simply ridge regression with λ = 0.
#???????
ridge.pred=predict(ridge.mod,s=0,newx=x[test,],exact=T) 
mean((ridge.pred-y.test)^2)

lm(y~x, subset=train)
predict(ridge.mod,s=0,exact=T,type="coefficients")[1:20,]
# we want to fit a (unpenalized) least squares model, then we should use the lm() function, since that function provides more useful outputs, such as standard errors and p-values for the coefficients.

# instead of arbitrarily choosing λ = 4, it would be better to use cross-validation to choose the tuning parameter λ
# can do this using the built-in cross-validation function, cv.glmnet().
# the function performs ten-fold cross-validation, though this can be changed using the argument nfolds.
#??????
set.seed(1)
cv.out=cv.glmnet(x[train ,],y[train],alpha=0)
plot(cv.out)
bestlam=cv.out$lambda.min
bestlam
# we see that the value of λ that results in the smallest cross- validation error

# What is the test MSE associated with this value of λ?
ridge.pred=predict(ridge.mod,s=bestlam ,newx=x[test,]) 
mean((ridge.pred-y.test)^2)
# represents a further improvement over the test MSE that we got using λ = 4.

# we refit our ridge regression model on the full data set, using the value of λ chosen by cross-validation, and examine the coefficient estimates.
out=glmnet(x,y,alpha=0)
predict(out,type="coefficients",s=bestlam)[1:20,]
# none of the coefficients are zero—ridge regression does not perform variable selection!


    # 6.6.2 The Lasso
# now ask whether the lasso can yield either a more accurate or a more interpretable model than ridge regression.
# In order to fit a lasso model, we once again use the glmnet() function; however, this time we use the argument alpha=1.
lasso.mod=glmnet(x[train ,],y[train],alpha=1,lambda=grid) 
plot(lasso.mod)
# We can see from the coefficient plot that depending on the choice of tuning parameter, some of the coefficients will be exactly equal to zero.

# We now perform cross-validation and compute the associated test error
#??????
set.seed(1) 
cv.out=cv.glmnet(x[train ,],y[train],alpha=1)
plot(cv.out)
bestlam=cv.out$lambda.min
lasso.pred=predict(lasso.mod,s=bestlam ,newx=x[test,])
mean((lasso.pred-y.test)^2)
# substantially lower than the test set MSE of the null model and of least squares, and very similar to the test MSE of ridge regression with λ chosen by cross-validation.

# the lasso has a substantial advantage over ridge regression in that the resulting coefficient estimates are sparse.
#  we see that 12 of the 19 coefficient estimates are exactly zero.
# the lasso model with λ chosen by cross-validation contains only seven variables.
out=glmnet(x,y,alpha=1,lambda=grid)
lasso.coef=predict(out,type="coefficients",s=bestlam)[1:20,] 
lasso.coef


        # 6.7 Lab 3: PCR and PLS Regression
    # 6.7.1 Principal Components Regression
# Principal components regression (PCR) can be performed using the pcr() function,which is part of the pls library
# now apply PCR to the Hitters data, in order to predict Salary. 
# Again, ensure that the missing values have been removed from the data, as described in Section 6.5.
library(pls)
set.seed(2)
pcr.fit=pcr(Salary~., data=Hitters ,scale=TRUE, validation ="CV")
# The syntax for the pcr() function is similar to that for lm(), with a few additional options.
# Setting scale=TRUE has the effect of standardizing each predictor, using (6.6), prior to generating the principal components, so that the scale on which each variable is measured will not have an effect.
# Setting validation="CV" causes pcr() to compute the ten-fold cross-validation error for each possible value of M , the number of principal components used
# The resulting fit can be examined using summary()
#??????
summary(pcr.fit)
# The CV score is provided for each possible number of components, ranging from M = 0 onwards.
# Note that pcr() reports the root mean squared error; in order to obtain the usual MSE, we must square this quantity.
# For instance, a root mean squared error of 352.8 corresponds to an MSE of 352.82 = 124,468.

# can also plot the cross-validation scores using the validationplot() function. 
# Using val.type="MSEP" will cause the cross-validation MSE to be plotted.
dev.new()
validationplot(pcr.fit,val.type="MSEP")
# We see that the smallest cross-validation error occurs when M = 16 com- ponents are used.
# This suggests that a model that uses just a small number of components might suffice.

# The summary() function also provides the percentage of variance explained in the predictors and in the response using different numbers of compo- nents.

# We now perform PCR on the training data and evaluate its test set performance.
set.seed(1)
pcr.fit=pcr(Salary~., data=Hitters,subset=train,scale=TRUE,validation ="CV")
validationplot(pcr.fit,val.type="MSEP")
# we find that the lowest cross-validation error occurs when M = 7 component are used

# compute the test MSE 
pcr.pred=predict(pcr.fit,x[test,],ncomp=7)
mean((pcr.pred-y.test)^2)
# This test set MSE is competitive with the results obtained using ridge re- gression and the lasso.

# we fit PCR on the full data set, using M = 7, the number of components identified by cross-validation.
pcr.fit=pcr(y~x,scale=TRUE,ncomp=7) 
summary(pcr.fit)


    # 6.7.2 Partial Least Squares
# implement partial least squares (PLS) using the plsr() function, also in the pls library. The syntax is just like that of the pcr() function.
set.seed(1)
pls.fit=plsr(Salary~., data=Hitters ,subset=train,scale=TRUE,validation ="CV")
summary(pls.fit)

validationplot(pls.fit,val.type="MSEP")
# The lowest cross-validation error occurs when only M = 2 partial least squares directions are used.

# now evaluate the corresponding test set MSE.
pls.pred=predict(pls.fit,x[test,],ncomp=2) 
mean((pls.pred-y.test)^2)
# The test MSE is comparable to, but slightly higher than, the test MSE obtained using ridge regression, the lasso, and PCR.

# we perform PLS using the full data set, using M = 2, the number of components identified by cross-validation.
pls.fit=plsr(Salary~., data=Hitters ,scale=TRUE,ncomp=2) 
summary(pls.fit)

# the percentage of variance in Salary that the two-component PLS fit explains, 46.40 %, is almost as much as that explained using the final seven-component model PCR fit, 46.69 %. 
# PCR only attempts to maximize the amount of variance explained in the predictors, while PLS searches for directions that explain variance in both the predic- tors and the response.
  


            # Applied Exercises

#Q9
library(ISLR)
fix(College)
names(College)
dim(College)

#a train/test Split
# (a) Split the data set into a training set and a test set.
set.seed(3)
train_index <- sample(1:nrow(College), round(nrow(College) * 0.7))

train <- College[train_index, ]
nrow(train) / nrow(College)

test <- College[-train_index, ]
nrow(test) / nrow(College)

#exclude accepted and enrolled
train <- College[train_index, -c(3,4)]
test <- College[-train_index, -c(3,4)]

#b  OLS Regression
# Fit a linear model using least squares on the training set, and report the test error obtained.
model_linear <- lm(Apps ~ ., data = train)
summary(model_linear)

model_linear <- lm(Apps ~ ., data = train )

ols_pred <- predict(model_linear, test)
(ols_mse <- mean((ols_pred - test$Apps)^2))


train_mat<-model.matrix(lm(Apps ~ ., data = train))
test_mat<-model.matrix(lm(Apps ~ ., data = test))

# c  Ridge Regression
# Fit a ridge regression model on the training set, with λ chosen by cross-validation. Report the test error obtained.
library(caret) #dummyvars
library(glmnet) # for ridge regression & lasso
library(dplyr)


set.seed(3)

model_ridge <- cv.glmnet(y = train$Apps, 
                         x = train_mat, 
                         alpha = 0, 
                         lambda = 10^seq(2,-2, length = 100), 
                         standardize = TRUE, 
                         nfolds = 5)
dev.new()
data.frame(lambda = model_ridge$lambda, 
           cv_mse = model_ridge$cvm) %>%
  ggplot(aes(x = lambda, y = cv_mse)) + 
  geom_point() + 
  geom_line() + 
  geom_vline(xintercept = model_ridge$lambda.min, col = "deepskyblue3") +
  geom_hline(yintercept = min(model_ridge$cvm), col = "deepskyblue3") +
  scale_x_continuous(trans = 'log10', breaks = c(0.01, 0.1, 1, 10, 100), labels = c(0.01, 0.1, 1, 10, 100)) + 
  scale_y_continuous(labels = scales::comma_format()) + 
  theme(legend.position = "bottom") + 
  labs(x = "Lambda", 
       y = "Cross-Validation MSE", 
       col = "Non-Zero Coefficients:", 
       title = "Ridge Regression - Lambda Selection (Using 5-Fold Cross-Validation)")


model_ridge_best <- glmnet(y = train$Apps,
                           x = train_mat,
                           alpha = 0, 
                           lambda = 10^seq(2,-2, length = 100))

ridge_pred <- predict(model_ridge_best, s = model_ridge$lambda.min, newx = test_mat)
(ridge_mse <- mean((ridge_pred - test$Apps)^2))


# (d) The Lasso
# Fit a lasso model on the training set, with λ chosen by cross- validation. Report the test error obtained, along with the number of non-zero coefficient estimates.
set.seed(4)

model_lasso <- cv.glmnet(y = train$Apps, x = train_mat, alpha = 1, lambda = 10^seq(2, -2, length = 100), standardize = TRUE, nfolds = 5, thresh = 1e-12)

data.frame(lambda = model_lasso$lambda, 
           cv_mse = model_lasso$cvm, 
           nonzero_coeff = model_lasso$nzero) %>%
  ggplot(aes(x = lambda, y = cv_mse, col = nonzero_coeff)) + 
  geom_point() + 
  geom_line() + 
  geom_vline(xintercept = model_lasso$lambda.min, col = "deepskyblue3") +
  geom_hline(yintercept = min(model_lasso$cvm), col = "deepskyblue3") +
  scale_x_continuous(trans = 'log10', breaks = c(0.01, 0.1, 1, 10, 100), labels = c(0.01, 0.1, 1, 10, 100)) + 
  scale_y_continuous(labels = scales::comma_format()) + 
  theme(legend.position = "bottom") + 
  scale_color_gradient(low = "red", high = "green") +
  labs(x = "Lambda", 
       y = "Cross-Validation MSE", 
       col = "Non-Zero Coefficients:", 
       title = "Lasso - Lambda Selection (Using 5-Fold Cross-Validation)")

model_lasso_best <- glmnet(y = train$Apps,
                           x = train_mat,
                           alpha = 1, 
                           lambda = 10^seq(2,-5, length = 100))

lasso_pred <- predict(model_lasso_best, s = model_lasso$lambda.min, newx = test_mat)
(lasso_mse <- mean((lasso_pred - test$Apps)^2))

lasso_coef <- predict(model_lasso_best, type = "coefficients", s = model_lasso$lambda.min)

round(lasso_coef, 3)

#  (e) Principal Components Regression
# Fit a PCR model on the training set, with M chosen by cross- validation. Report the test error obtained, along with the value of M selected by cross-validation.
library(pls)
set.seed(5)

model_pcr <- pcr(Apps ~ .,data = train, scale = T, validation = "CV")

model_pcr_mse <- MSEP(model_pcr, estimate = "CV")$val %>%
  reshape2::melt() %>%
  mutate(M = 0:(nrow(.)-1)) %>%
  select(M, value) %>%
  mutate(CV_MSE = value) 

model_pcr_mse

model_pcr_mse %>%
  mutate(min_CV_MSE = as.numeric(min(CV_MSE) == CV_MSE)) %>%
  ggplot(aes(x = M, y = CV_MSE)) + 
  geom_line(col = "grey55") + 
  geom_point(size = 2, aes(col = factor(min_CV_MSE))) + 
  scale_y_continuous(labels = scales::comma_format()) + 
  scale_color_manual(values = c("deepskyblue3", "green")) + 
  theme(legend.position = "none") + 
  labs(x = "M", 
       y = "Cross-Validation MSE", 
       col = "Non-Zero Coefficients:", 
       title = "PCR - M Selection (Using 10-Fold Cross-Validation)")

pcr_pred <- predict(model_pcr, test, ncomp = 12.5)
(pcr_mse <- mean((pcr_pred - test$Apps)^2))


# (f) Partial Least Squares Regression
# Fit a PLS model on the training set, with M chosen by cross- validation. Report the test error obtained, along with the value of M selected by cross-validation.
set.seed(6)

model_pls <- plsr(Apps ~ .,
                  data = train, 
                  scale = T, 
                  validation = "CV")

model_pls_mse <- MSEP(model_pls, estimate = "CV")$val %>%
  reshape2::melt() %>%
  mutate(M = 0:(nrow(.)-1)) %>%
  select(M, value) %>%
  rename(CV_MSE = value)

model_pls_mse

model_pls_mse %>%
  mutate(min_CV_MSE = as.numeric(min(CV_MSE) == CV_MSE)) %>%
  ggplot(aes(x = M, y = CV_MSE)) + 
  geom_line(col = "grey55") + 
  geom_point(size = 2, aes(col = factor(min_CV_MSE))) + 
  scale_y_continuous(labels = scales::comma_format()) + 
  scale_color_manual(values = c("deepskyblue3", "green")) + 
  theme(legend.position = "none") + 
  labs(x = "M", 
       y = "Cross-Validation MSE", 
       title = "PLS - M Selection (Using 10-Fold Cross-Validation)")

pls_pred <- predict(model_pls, test, ncomp = 6)
(pls_mse <- mean((pls_pred - test$Apps)^2))


# (g) Performance Comparison
# Comment on the results obtained. How accurately can we pre- dict the number of college applications received? Is there much difference among the test errors resulting from these five ap- proaches?
# R2 = 1 - (SS_res / SS_tot)^2

SS_tot <- sum((test$Apps - mean(test$Apps))^2)

data.frame(method = c("OLS", "Ridge", "Lasso", "PCR", "PLS"), 
           test_MSE = c(ols_mse, ridge_mse, lasso_mse, pcr_mse, pls_mse), 
           test_R2 = c(1 - sum((test$Apps - ols_pred)^2) / SS_tot,
                       1 - sum((test$Apps - ridge_pred)^2) / SS_tot, 
                       1 - sum((test$Apps - lasso_pred)^2) / SS_tot, 
                       1 - sum((test$Apps - pcr_pred)^2) / SS_tot, 
                       1 - sum((test$Apps - pls_pred)^2) / SS_tot)) %>%
  arrange(test_MSE)  



#11
library(caret)#dummyvars
library(glmnet) # for ridge regression & lasso
library(dplyr)
library(plyr)
library(leaps)
library(MASS)
data(Boston)

#a 
# train/test Split
# Split the data set into a training set and a test set.
set.seed(4)
train_index <- sample(1:nrow(Boston), round(nrow(Boston) * 0.6))

train <- Boston[train_index, ]
nrow(train) / nrow(Boston)

test <- Boston[-train_index, ]
nrow(test) / nrow(Boston)


    #OLS Regression best subset selection
# Fit a linear model using least squares on the training set, and report the test error obtained.
model_linear <- lm(crim~ ., data = train)
summary(model_linear)
model_linear <- lm(crim~ ., data = train )
ols_pred <- predict(model_linear, test)
(ols_mse <- mean((ols_pred - test$crim)^2))


train_mat<-model.matrix(lm(crim~ ., data = train))
test_mat<-model.matrix(lm(crim~ ., data = test))

    # Ridge Regression
# Fit a ridge regression model on the training set, with λ chosen by cross-validation. Report the test error obtained.
library(caret) #dummyvars
library(glmnet) # for ridge regression & lasso
library(dplyr)

set.seed(4)
model_ridge <- cv.glmnet(y = train$crim, 
                         x = train_mat, 
                         alpha = 0, 
                         lambda = 10^seq(2,-2, length = 100), 
                         standardize = TRUE, 
                         nfolds = 5)

data.frame(lambda = model_ridge$lambda, 
           cv_mse = model_ridge$cvm) %>%
  ggplot(aes(x = lambda, y = cv_mse)) + 
  geom_point() + 
  geom_line() + 
  geom_vline(xintercept = model_ridge$lambda.min, col = "deepskyblue3") +
  geom_hline(yintercept = min(model_ridge$cvm), col = "deepskyblue3") +
  scale_x_continuous(trans = 'log10', breaks = c(0.01, 0.1, 1, 10, 100), labels = c(0.01, 0.1, 1, 10, 100)) + 
  scale_y_continuous(labels = scales::comma_format()) + 
  theme(legend.position = "bottom") + 
  labs(x = "Lambda", 
       y = "Cross-Validation MSE", 
       col = "Non-Zero Coefficients:", 
       title = "Ridge Regression - Lambda Selection (Using 5-Fold Cross-Validation)")


model_ridge_best <- glmnet(y = train$crim,
                           x = train_mat,
                           alpha = 0, 
                           lambda = 10^seq(2,-2, length = 100))

ridge_pred <- predict(model_ridge_best, s = model_ridge$lambda.min, newx = test_mat)
(ridge_mse <- mean((ridge_pred - test$crim)^2))


          # The Lasso
# Fit a lasso model on the training set, with λ chosen by cross- validation. Report the test error obtained, along with the number of non-zero coefficient estimates.
set.seed(4)

model_lasso <- cv.glmnet(y = train$crim, x = train_mat, alpha = 1, lambda = 10^seq(2, -2, length = 100), standardize = TRUE, nfolds = 5, thresh = 1e-12)

data.frame(lambda = model_lasso$lambda, 
           cv_mse = model_lasso$cvm, 
           nonzero_coeff = model_lasso$nzero) %>%
  ggplot(aes(x = lambda, y = cv_mse, col = nonzero_coeff)) + 
  geom_point() + 
  geom_line() + 
  geom_vline(xintercept = model_lasso$lambda.min, col = "deepskyblue3") +
  geom_hline(yintercept = min(model_lasso$cvm), col = "deepskyblue3") +
  scale_x_continuous(trans = 'log10', breaks = c(0.01, 0.1, 1, 10, 100), labels = c(0.01, 0.1, 1, 10, 100)) + 
  scale_y_continuous(labels = scales::comma_format()) + 
  theme(legend.position = "bottom") + 
  scale_color_gradient(low = "red", high = "green") +
  labs(x = "Lambda", 
       y = "Cross-Validation MSE", 
       col = "Non-Zero Coefficients:", 
       title = "Lasso - Lambda Selection (Using 5-Fold Cross-Validation)")

model_lasso_best <- glmnet(y = train$crim,
                           x = train_mat,
                           alpha = 1, 
                           lambda = 10^seq(2,-5, length = 100))

lasso_pred <- predict(model_lasso_best, s = model_lasso$lambda.min, newx = test_mat)
(lasso_mse <- mean((lasso_pred - test$crim)^2))

lasso_coef <- predict(model_lasso_best, type = "coefficients", s = model_lasso$lambda.min)
round(lasso_coef, 3)

        #Principal Components Regression
# Fit a PCR model on the training set, with M chosen by cross- validation. Report the test error obtained, along with the value of M selected by cross-validation.
library(pls)
set.seed(4)
model_pcr <- pcr(crim~ .,data = train, scale = T, validation = "CV")
model_pcr_mse <- MSEP(model_pcr, estimate = "CV")$val %>%
  reshape2::melt() %>%
  mutate(M = 0:(nrow(.)-1)) %>%
  select(M, value) %>%
  mutate(CV_MSE = value)

model_pcr_mse

model_pcr_mse %>%
  mutate(min_CV_MSE = as.numeric(min(CV_MSE) == CV_MSE)) %>%
  ggplot(aes(x = M, y = CV_MSE)) + 
  geom_line(col = "grey55") + 
  geom_point(size = 2, aes(col = factor(min_CV_MSE))) + 
  scale_y_continuous(labels = scales::comma_format()) + 
  scale_color_manual(values = c("deepskyblue3", "green")) + 
  theme(legend.position = "none") + 
  labs(x = "M", 
       y = "Cross-Validation MSE", 
       col = "Non-Zero Coefficients:", 
       title = "PCR - M Selection (Using 10-Fold Cross-Validation)")

pcr_pred <- predict(model_pcr, test, ncomp = 13.5)
(pcr_mse <- mean((pcr_pred - test$crim)^2))


        # Performance Comparison
# Comment on the results obtained. How accurately can we pre- dict the number of college applications received? Is there much difference among the test errors resulting from these five ap- proaches?
# R2 = 1 - (SS_res / SS_tot)^2

SS_tot <- sum((test$crim - mean(test$crim))^2)

data.frame(method = c("OLS", "Ridge", "Lasso", "PCR"), 
           test_MSE = c(ols_mse, ridge_mse, lasso_mse, pcr_mse), 
           test_R2 = c(1 - sum((test$crim - ols_pred)^2) / SS_tot,
                       1 - sum((test$crim - ridge_pred)^2) / SS_tot, 
                       1 - sum((test$crim - lasso_pred)^2) / SS_tot, 
                       1 - sum((test$crim - pcr_pred)^2) / SS_tot)) %>%
  arrange(test_MSE)  

#b Propose a model (or set of models) that seem to perform well on this data set, and justify your answer using cross- validation
set.seed(4)
library(boot)
K=10
cv.error=rep(0,10)
for (i in 1:K){
  glm.fit=glm(crim~.poly(.,i),data=Boston)
  cv.error[i]=cv.glm(Boston,glm.fit,K=10)$delta[1]
}
cv.error



#############################################################################
ctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 10, summaryFunction = defaultSummary)
# Best Subset Selection:
# best models per subset size:
model <- regsubsets(crim ~ ., 
                    data = Boston, 
                    nvmax = ncol(Boston) - 1, 
                    method = "exhaustive")




# cross-validating to compare MSE:

CV_MSE <- c()

set.seed(10101)

for (i in 1:(ncol(Boston)-1)) {
  Boston_temp <- Boston[ ,c("crim", names(coef(model, id = i)[-1]))]
  model_temp <- train(crim ~ ., 
                      data = Boston_temp, 
                      method = "lm", 
                      trControl = ctrl)
  CV_MSE[i] <- model_temp$results$MSE
}

# ?????
data.frame(CV_MSE = CV_MSE, subset_size = 1:(ncol(Boston)-1)) %>%
  mutate(min_CV_MSE = as.numeric(min(CV_MSE) == CV_MSE)) %>%
  ggplot(aes(x = subset_size, y = CV_MSE)) + 
  geom_line(col = "grey55") + 
  geom_point(size = 2, aes(col = factor(min_CV_MSE))) + 
  scale_x_continuous(breaks = seq(1, ncol(Boston)-1), minor_breaks = NULL) +
  scale_color_manual(values = c("deepskyblue3", "green")) + 
  theme(legend.position = "none") + 
  labs(title = "Boston Dataset - Best Subset Selection", 
       subtitle = "Selecting parameter subset size with cross-validation",
       x = "Subset Size", 
       y = "CV MSE")

min(CV_MSE)

# Lasso Regression:
set.seed(159)

model_lasso <- train(crim ~ ., 
                     data = Boston, 
                     method = "glmnet", 
                     preProcess = c("center", "scale"), 
                     metric = "MSE",
                     maximize = F,
                     trControl = ctrl, 
                     tuneGrid = expand.grid(alpha = 1,
                                            lambda = seq(0, 1, length = 100)))



model_lasso$results %>%
  rename(CV_MSE = MSE) %>%
  mutate(min_CV_MSE = as.numeric(lambda == model_lasso$bestTune$lambda)) %>%
  ggplot(aes(x = lambda, y = CV_MSE)) + 
  geom_line(col = "grey55") + 
  geom_point(size = 2, aes(col = factor(min_CV_MSE))) + 
  scale_color_manual(values = c("deepskyblue3", "green")) + 
  theme(legend.position = "none") + 
  labs(title = "Boston Dataset - Lasso Regression", 
       subtitle = "Selecting shrinkage parameter with cross-validation",
       y = "CV MSE")

min(model_lasso$results$MSE)


# Ridge Regression:


model_ridge <- train(crim ~ ., 
                     data = Boston, 
                     method = "glmnet", 
                     preProcess = c("center", "scale"), 
                     metric = "MSE",
                     maximize = F,
                     trControl = ctrl, 
                     tuneGrid = expand.grid(alpha = 0,
                                            lambda = seq(0, 1, length = 100)))


model_ridge$results %>%
  rename(CV_MSE = MSE) %>%
  mutate(min_CV_MSE = as.numeric(lambda == model_ridge$bestTune$lambda)) %>%
  ggplot(aes(x = lambda, y = CV_MSE)) + 
  geom_line(col = "grey55") + 
  geom_point(size = 2, aes(col = factor(min_CV_MSE))) + 
  scale_color_manual(values = c("deepskyblue3", "green")) + 
  theme(legend.position = "none") + 
  labs(title = "Boston Dataset - Ridge Regression", 
       subtitle = "Selecting shrinkage parameter with cross-validation",
       y = "CV MSE")

min(model_ridge$results$MSE)


# Principal Components Regression:
set.seed(339)

model_pcr <- train(crim ~ ., 
                   data = Boston, 
                   method = "pcr", 
                   preProcess = c("center", "scale"), 
                   metric = "MSE", 
                   maximize = F,
                   trControl = ctrl, 
                   tuneGrid = expand.grid(ncomp = 1:(ncol(Boston)-1)))


model_pcr$results %>%
  rename(CV_MSE = MSE) %>%
  mutate(min_CV_MSE = as.numeric(ncomp == model_pcr$bestTune$ncomp)) %>%
  ggplot(aes(x = ncomp, y = CV_MSE)) + 
  geom_line(col = "grey55") + 
  geom_point(size = 2, aes(col = factor(min_CV_MSE))) + 
  scale_x_continuous(breaks = seq(1, ncol(Boston)-1), minor_breaks = NULL) +
  scale_color_manual(values = c("deepskyblue3", "green")) + 
  theme(legend.position = "none") + 
  labs(title = "Boston Dataset - Principal Components Regression", 
       subtitle = "Selecting number of principal components with cross-validation",
       x = "Principal Components", 
       y = "CV MSE")

min(model_pcr$results$MSE)


# Partial Least Squares:
set.seed(840)

model_pls <- train(crim ~ ., 
                   data = Boston, 
                   method = "pls", 
                   preProcess = c("center", "scale"), 
                   metric = "MSE", 
                   maximize = F,
                   trControl = ctrl, 
                   tuneGrid = expand.grid(ncomp = 1:(ncol(Boston)-1)))


model_pls$results %>%
  rename(CV_MSE = MSE) %>%
  mutate(min_CV_MSE = as.numeric(ncomp == model_pls$bestTune$ncomp)) %>%
  ggplot(aes(x = ncomp, y = CV_MSE)) + 
  geom_line(col = "grey55") + 
  geom_point(size = 2, aes(col = factor(min_CV_MSE))) + 
  scale_x_continuous(breaks = seq(1, ncol(Boston)-1), minor_breaks = NULL) +
  scale_color_manual(values = c("deepskyblue3", "green")) + 
  theme(legend.position = "none") + 
  labs(title = "Boston Dataset - Partial Least Squares", 
       subtitle = "Selecting number of principal components with cross-validation",
       x = "Principal Components", 
       y = "CV MSE")

min(model_pls$results$MSE)


# (b) Proposed Model
Boston$medv_sq <- Boston$medv^2
Boston$medv_cub <- Boston$medv^3

Boston$rad_cat <- ifelse(Boston$rad > 20, 1, 0)
Boston$rad <- NULL
Boston$tax <- NULL

glimpse(Boston)

# best models per subset size:
model <- regsubsets(crim ~ ., 
                    data = Boston, 
                    nvmax = ncol(Boston) - 1, 
                    method = "exhaustive")


# cross-validating to compare MSE:
CV_MSE <- c()

set.seed(20202)

for (i in 1:(ncol(Boston)-1)) {
  Boston_temp <- Boston[ ,c("crim", names(coef(model, id = i)[-1]))]
  model_temp <- train(crim ~ ., 
                      data = Boston_temp, 
                      method = "lm", 
                      trControl = ctrl)
  CV_MSE[i] <- model_temp$results$MSE
}

data.frame(CV_MSE = CV_MSE, subset_size = 1:(ncol(Boston)-1)) %>%
  mutate(min_CV_MSE = as.numeric(min(CV_MSE) == CV_MSE)) %>%
  ggplot(aes(x = subset_size, y = CV_MSE)) + 
  geom_line(col = "grey55") + 
  geom_point(size = 2, aes(col = factor(min_CV_MSE))) + 
  scale_x_continuous(breaks = seq(1, ncol(Boston)-1), minor_breaks = NULL) +
  scale_color_manual(values = c("deepskyblue3", "green")) + 
  theme(legend.position = "none") + 
  labs(title = "Modified Boston Dataset - Best Subset Selection", 
       subtitle = "Selecting parameter subset size with cross-validation",
       x = "Subset Size", 
       y = "CV MSE")


# (c) Selected Model
coef(model, id = 7)

coef(model, id = 4)
