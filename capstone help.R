#make new training and test removing non-signifcant variables
train2 <- scrap_price[train_index, -c(1,2,4,5,6,8,9,10,11,14,15,16,18,19,21,22,24,25)]
nrow(train) / nrow(scrap_price)

test2 <- scrap_price[-train_index, -c(1,2,4,5,6,8,9,10,11,14,15,16,18,19,21,22,24,25)]
nrow(test) / nrow(scrap_price)

model_linear2 <- lm(price~ ., data = train2)
summary(model_linear2)    

ols_pred2 <- predict(model_linear2, test2)
(ols_mse <- mean((ols_pred2 - scrap_price$price)^2))
# mse after removing non-significant variables
# 118014356



# change the rejected variable. reject of pvalue < or = to 0.05

# reference for boxplots
boxplot(year ~ mpg01, data = Auto, main = "Year vs mpg01")

###########################################################
tree.scrap=tree(price~., data = train)
summary(tree.scrap)
plot(tree.scrap)
text(tree.scrap,pretty=0)

# use the cv.tree() function to see whether pruning the tree will improve performance
cv.scrap=cv.tree(tree.scrap)
plot(cv.scrap$size ,cv.scrap$dev ,type='b')

# MSE from CART
scrapyhat=predict(tree.scrap ,newdata=test)
plot(scrapyhat,test_maty)
abline(0,1)
mean((scrapyhat-test_maty)^2)


# the most complex tree is selected by cross-validation. How- ever, if we wish to prune the tree, we could do so as follows, using the prune.tree() function
prune.scrap=prune.tree(tree.scrap,best=3)
plot(prune.scrap)
text(prune.scrap ,pretty=0)

scrapyhatprune=predict(prune.scrap ,newdata=test) 
plot(scrapyhatprune,test_maty)
abline(0,1)
mean((scrapyhatprune-test_maty)^2)
######################################################

# use means for test error and compare. add lda, qda,

# The most common interpretation of r-squared is how well the regression model explains observed data. 
# For example, an r-squared of 60% reveals that 60% of the variability observed in the target variable is explained by the regression model.



################################### Without Removing Non-significant Variables #################################################
#Splitting test and training sets of data
set.seed(1010)
train_index <- sample(1:nrow(scrap_price), round(nrow(scrap_price) * 0.7))

train <- scrap_price[train_index, -c(1,2,9)]
nrow(train) / nrow(scrap_price)

test <- scrap_price[-train_index, -c(1,2,9)]
nrow(test) / nrow(scrap_price)
# enginelocation, ID, symboling  must be removed because no significant to start after exploratory statistics


# OLS Regression
# Fit a linear model using least squares on the training set, and report the test error obtained.
model_linear <- lm(price~ ., data = train)
summary(model_linear)              
# p values for fueltypes, aspiration, doornumbers, drivewheels, wheelbase, carlength, curbweight, enginetype, cylindernumber, 
# fuelsystem, boreratio, compressionratio, horsepower, citympg, highwaympg  > 0.5  not statistically significant

#use MSE as the test error metric
ols_pred <- predict(model_linear, test)
(ols_mse <- mean((ols_pred - scrap_price$price)^2))
# mse before removing non-significant variables
# 116930647




train_mat<-model.matrix(lm(price ~ ., data = train ))

# Ridge Regression
# Fit a ridge regression model on the training set, with λ chosen by cross-validation. Report the test error obtained.
library(caret) #dummyvars
library(glmnet) # for ridge regression & lasso
library(ISLR)
library(glmnet)
library(dplyr)
library(ggplot2)
library(colorspace)

set.seed(1010)

train_mat <- dummyVars(price~ ., data = train, fullRank = F) %>%
  predict(newdata = train) %>%
  as.matrix()

test_mat <- dummyVars(price ~ ., data = test, fullRank = F) %>%
  predict(newdata = test) %>%
  as.matrix()

#changes lambda between 10^-4 and 4
model_ridge <- cv.glmnet(y = train$price, 
                         x = train_mat, 
                         alpha = 0, 
                         lambda = 10^seq(4,-4, length = 100), 
                         standardize = TRUE, 
                         nfolds = 5)

dev.off()

data.frame(lambda = model_ridge$lambda, 
           cv_mse = model_ridge$cvm) %>%
  ggplot(aes(x = lambda, y = cv_mse)) + 
  geom_point() + 
  geom_line() + 
  geom_vline(xintercept = model_ridge$lambda.min, col = "deepskyblue3") +
  geom_hline(yintercept = min(model_ridge$cvm), col = "deepskyblue3") +
  scale_x_continuous(trans = 'log10', breaks = c(0.0001,0.001,0.01, 0.1, 1, 10, 100, 1000, 10000), labels = c(0.0001,0.001,0.01, 0.1, 1, 10, 100, 1000, 10000)) + 
  scale_y_continuous(labels = scales::comma_format()) + 
  theme(legend.position = "bottom") + 
  labs(x = "Lambda", 
       y = "Cross-Validation MSE", 
       col = "Non-Zero Coefficients:", 
       title = "Ridge Regression - Lambda Selection (Using 5-Fold Cross-Validation)")
#changes lambda between 10^-4 and 4

model_ridge_best <- glmnet(y = train$price,
                           x = train_mat,
                           alpha = 0, 
                           lambda = 10^seq(4,-4, length = 100))

ridge_pred <- predict(model_ridge_best, s = model_ridge$lambda.min, newx = test_mat) #????? must be 22 variable for newx
(ridge_mse <- mean((ridge_pred - test$price)^2))

#coefficients 
ridge_coef <- predict(model_ridge_best, type = "coefficients", s = model_ridge$lambda.min)
round(ridge_coef, 3)

# The Lasso
# Fit a lasso model on the training set, with λ chosen by cross- validation. Report the test error obtained, along with the number of non-zero coefficient estimates.
set.seed(1010)

model_lasso <- cv.glmnet(y = train$price, x = train_mat, alpha = 1, lambda = 10^seq(4, -4, length = 100), standardize = TRUE, nfolds = 5, thresh = 1e-12)

data.frame(lambda = model_lasso$lambda, 
           cv_mse = model_lasso$cvm, 
           nonzero_coeff = model_lasso$nzero) %>%
  ggplot(aes(x = lambda, y = cv_mse, col = nonzero_coeff)) + 
  geom_point() + 
  geom_line() + 
  geom_vline(xintercept = model_lasso$lambda.min, col = "deepskyblue3") +
  geom_hline(yintercept = min(model_lasso$cvm), col = "deepskyblue3") +
  scale_x_continuous(trans = 'log10', breaks = c(0.0001,0.001,0.01, 0.1, 1, 10, 100, 1000, 10000), labels = c(0.0001,0.001,0.01, 0.1, 1, 10, 100, 1000, 10000)) + 
  scale_y_continuous(labels = scales::comma_format()) + 
  theme(legend.position = "bottom") + 
  scale_color_gradient(low = "red", high = "green") +
  labs(x = "Lambda", 
       y = "Cross-Validation MSE", 
       col = "Non-Zero Coefficients:", 
       title = "Lasso - Lambda Selection (Using 5-Fold Cross-Validation)")

model_lasso_best <- glmnet(y = train$price,
                           x = train_mat,
                           alpha = 1, 
                           lambda = 10^seq(4,-4, length = 100))

lasso_pred <- predict(model_lasso_best, s = model_lasso$lambda.min, newx = test_mat)
(lasso_mse <- mean((lasso_pred - test$price)^2))

#coefficients 
lasso_coef <- predict(model_lasso_best, type = "coefficients", s = model_lasso$lambda.min)
round(lasso_coef, 3)


# Principal Components Regression
# Fit a PCR model on the training set, with M chosen by cross- validation. Report the test error obtained, along with the value of M selected by cross-validation.
library(pls)
set.seed(1010)

model_pcr <- pcr(price ~ .,data = train, scale = T, validation = "CV")

model_pcr_mse <- MSEP(model_pcr, estimate = "CV")$val %>%
  reshape2::melt() %>%
  mutate(M = 0:(nrow(.)-1)) %>%
  select(M, value) %>%
  mutate(CV_MSE = value) #?????? unused argument (CV_MSE = value)

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

pcr_pred <- predict(model_pcr, test, ncomp = 11) #????? need ncomp from CV
(pcr_mse <- mean((pcr_pred - test$price)^2))


# Partial Least Squares Regression
# Fit a PLS model on the training set, with M chosen by cross- validation. Report the test error obtained, along with the value of M selected by cross-validation.
set.seed(1010)

model_pls <- plsr(price ~ .,
                  data = train, 
                  scale = T, 
                  validation = "CV")

model_pls_mse <- MSEP(model_pls, estimate = "CV")$val %>%
  reshape2::melt() %>%
  mutate(M = 0:(nrow(.)-1)) %>%
  select(M, value) %>%
  mutate(CV_MSE = value)

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

pls_pred <- predict(model_pls, test, ncomp = 7) #using ncomp=7 from CV gives lowest MSE
(pls_mse <- mean((pls_pred - test$price)^2))


# Performance Comparison
SS_tot <- sum((test$price - mean(test$price))^2)

data.frame(method = c("OLS", "Ridge", "Lasso", "PCR", "PLS"), 
           test_MSE = c(ols_mse, ridge_mse, lasso_mse, pcr_mse, pls_mse), 
           test_R2 = c(1 - sum((test$price - ols_pred)^2) / SS_tot,
                       1 - sum((test$price - ridge_pred)^2) / SS_tot, 
                       1 - sum((test$price - lasso_pred)^2) / SS_tot, 
                       1 - sum((test$price - pcr_pred)^2) / SS_tot, 
                       1 - sum((test$price - pls_pred)^2) / SS_tot)) %>%
  arrange(test_MSE)  

library(ggplot2)

######### boxplot
data = data.frame(A=(pcr_pred - test$price)^2,
                  B=(pls_pred - test$price)^2,
                  C=(lasso_pred - test$price)^2,
                  D=(ridge_pred - test$price)^2,
                  E=(ols_pred - test$price)^2)
library(ggplot2)
boxplot(data)
#########################################################################################################################


##############################################################################################
#Removing non-significant Variables
# p values for fueltypes, aspiration, doornumbers, drivewheels, wheelbase, carlength, curbweight, enginetype, cylindernumber, 
# fuelsystem, boreratio, compressionratio, horsepower, citympg, highwaympg  > 0.5  not statistically significant

#Splitting test and training sets of data
set.seed(1010)
train_index <- sample(1:nrow(scrap_price), round(nrow(scrap_price) * 0.7))

train2 <- scrap_price[train_index, -c(1,2,4,5,6,8,9,10,11,14,15,16,18,19,21,22,24,25)]
nrow(train) / nrow(scrap_price)

test2 <- scrap_price[-train_index, -c(1,2,4,5,6,8,9,10,11,14,15,16,18,19,21,22,24,25)]
nrow(test) / nrow(scrap_price)



# OLS Regression
# Fit a linear model using least squares on the training set, and report the test error obtained.
model_linear2 <- lm(price~ ., data = train2)
summary(model_linear2)              


#use MSE as the test error metric
ols_pred2 <- predict(model_linear2, test2)
(ols_mse2 <- mean((ols_pred2 - scrap_price$price)^2))

# mse = 118014356


train_mat2 <-model.matrix(lm(price ~ ., data = train2))

# Ridge Regression
# Fit a ridge regression model on the training set, with λ chosen by cross-validation. Report the test error obtained.

set.seed(1010)

train_mat2 <- dummyVars(price~ ., data = train2, fullRank = F) %>%
  predict(newdata = train2) %>%
  as.matrix()

test_mat2 <- dummyVars(price ~ ., data = test2, fullRank = F) %>%
  predict(newdata = test) %>%
  as.matrix()

#changes lambda between 10^-4 and 4
model_ridge2 <- cv.glmnet(y = train2$price, 
                          x = train_mat2, 
                          alpha = 0, 
                          lambda = 10^seq(4,-4, length = 100), 
                          standardize = TRUE, 
                          nfolds = 5)

dev.off()

data.frame(lambda = model_ridge2$lambda, 
           cv_mse = model_ridge2$cvm) %>%
  ggplot(aes(x = lambda, y = cv_mse)) + 
  geom_point() + 
  geom_line() + 
  geom_vline(xintercept = model_ridge2$lambda.min, col = "deepskyblue3") +
  geom_hline(yintercept = min(model_ridge2$cvm), col = "deepskyblue3") +
  scale_x_continuous(trans = 'log10', breaks = c(0.0001,0.001,0.01, 0.1, 1, 10, 100, 1000, 10000), labels = c(0.0001,0.001,0.01, 0.1, 1, 10, 100, 1000, 10000)) + 
  scale_y_continuous(labels = scales::comma_format()) + 
  theme(legend.position = "bottom") + 
  labs(x = "Lambda", 
       y = "Cross-Validation MSE", 
       col = "Non-Zero Coefficients:", 
       title = "Ridge Regression - Lambda Selection (Using 5-Fold Cross-Validation)")
#changes lambda between 10^-4 and 4

model_ridge_best2 <- glmnet(y = train2$price,
                            x = train_mat2,
                            alpha = 0, 
                            lambda = 10^seq(4,-4, length = 100))

ridge_pred2 <- predict(model_ridge_best2, s = model_ridge2$lambda.min, newx = test_mat2) #????? must be 22 variable for newx
(ridge_mse2 <- mean((ridge_pred2 - test2$price)^2))

#coefficients 
ridge_coef2 <- predict(model_ridge_best2, type = "coefficients", s = model_ridge2$lambda.min)
round(ridge_coef2, 3)



# The Lasso
# Fit a lasso model on the training set, with λ chosen by cross- validation. Report the test error obtained, along with the number of non-zero coefficient estimates.
set.seed(1010)

model_lasso2 <- cv.glmnet(y = train2$price, x = train_mat2, alpha = 1, lambda = 10^seq(4, -4, length = 100), standardize = TRUE, nfolds = 5, thresh = 1e-12)

data.frame(lambda = model_lasso2$lambda, 
           cv_mse = model_lasso2$cvm, 
           nonzero_coeff = model_lasso2$nzero) %>%
  ggplot(aes(x = lambda, y = cv_mse, col = nonzero_coeff)) + 
  geom_point() + 
  geom_line() + 
  geom_vline(xintercept = model_lasso2$lambda.min, col = "deepskyblue3") +
  geom_hline(yintercept = min(model_lasso2$cvm), col = "deepskyblue3") +
  scale_x_continuous(trans = 'log10', breaks = c(0.0001,0.001,0.01, 0.1, 1, 10, 100, 1000, 10000), labels = c(0.0001,0.001,0.01, 0.1, 1, 10, 100, 1000, 10000)) + 
  scale_y_continuous(labels = scales::comma_format()) + 
  theme(legend.position = "bottom") + 
  scale_color_gradient(low = "red", high = "green") +
  labs(x = "Lambda", 
       y = "Cross-Validation MSE", 
       col = "Non-Zero Coefficients:", 
       title = "Lasso - Lambda Selection (Using 5-Fold Cross-Validation)")

model_lasso_best2 <- glmnet(y = train2$price,
                            x = train_mat2,
                            alpha = 1, 
                            lambda = 10^seq(4,-4, length = 100))

lasso_pred2 <- predict(model_lasso_best2, s = model_lasso2$lambda.min, newx = test_mat2)
(lasso_mse2 <- mean((lasso_pred2 - test2$price)^2))

#coefficients 
lasso_coef2 <- predict(model_lasso_best2, type = "coefficients", s = model_lasso2$lambda.min)
round(lasso_coef2, 3)


# Principal Components Regression
# Fit a PCR model on the training set, with M chosen by cross- validation. Report the test error obtained, along with the value of M selected by cross-validation.
library(pls)
set.seed(1010)

model_pcr2 <- pcr(price ~ .,data = train2, scale = T, validation = "CV")

model_pcr_mse2 <- MSEP(model_pcr2, estimate = "CV")$val %>%
  reshape2::melt() %>%
  mutate(M = 0:(nrow(.)-1)) %>%
  select(M, value) %>%
  mutate(CV_MSE = value) #?????? unused argument (CV_MSE = value)

model_pcr_mse2

model_pcr_mse2 %>%
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

pcr_pred2 <- predict(model_pcr2, test, ncomp = 7) #????? need ncomp from CV
(pcr_mse2 <- mean((pcr_pred2 - test2$price)^2))


# Partial Least Squares Regression
# Fit a PLS model on the training set, with M chosen by cross- validation. Report the test error obtained, along with the value of M selected by cross-validation.
set.seed(1010)

model_pls2 <- plsr(price ~ .,
                   data = train2, 
                   scale = T, 
                   validation = "CV")

model_pls_mse2 <- MSEP(model_pls2, estimate = "CV")$val %>%
  reshape2::melt() %>%
  mutate(M = 0:(nrow(.)-1)) %>%
  select(M, value) %>%
  mutate(CV_MSE = value)

model_pls_mse2

model_pls_mse2 %>%
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

pls_pred2 <- predict(model_pls2, test2, ncomp = 4) #using ncomp=4 from CV gives lowest MSE
(pls_mse2 <- mean((pls_pred2 - test2$price)^2))


# Performance Comparison
SS_tot2 <- sum((test2$price - mean(test2$price))^2)

data.frame(method = c("OLS", "Ridge", "Lasso", "PCR", "PLS"), 
           test_MSE = c(ols_mse2, ridge_mse2, lasso_mse2, pcr_mse2, pls_mse2), 
           test_R2 = c(1 - sum((test2$price - ols_pred2)^2) / SS_tot2,
                       1 - sum((test2$price - ridge_pred2)^2) / SS_tot2, 
                       1 - sum((test2$price - lasso_pred2)^2) / SS_tot2, 
                       1 - sum((test2$price - pcr_pred2)^2) / SS_tot2, 
                       1 - sum((test2$price - pls_pred2)^2) / SS_tot2)) %>%
  arrange(test_MSE)  


######### boxplot
data = data.frame(A=(pcr_pred2 - test2$price)^2,
                  B=(pls_pred2 - test2$price)^2,
                  C=(lasso_pred2 - test2$price)^2,
                  D=(ridge_pred2 - test2$price)^2,
                  E=(ols_pred2 - test2$price)^2)
library(ggplot2)
boxplot(data)
###############################################################################################

#use MSE as the test error metric
ols_pred <- predict(model_linear, test)
ols_mse <- mse(ols_pred,test$price)



# Ridge Regression
# Fit a ridge regression model on the training set, with λ chosen by cross-validation. Report the test error obtained.
library(caret) #dummyvars
library(glmnet) # for ridge regression & lasso
library(ISLR)
library(glmnet)
library(dplyr)
library(ggplot2)
library(colorspace)

set.seed(1010)

#changes lambda between 10^-4 and 4
model_ridge <- cv.glmnet(train_matx,train_maty,alpha = 0,lambda = 10^seq(4,-4, length = 100),standardize = TRUE,nfolds = 5)

dev.off()

data.frame(lambda = model_ridge$lambda, 
           cv_mse = model_ridge$cvm) %>%
  ggplot(aes(x = lambda, y = cv_mse)) + 
  geom_point() + 
  geom_line() + 
  geom_vline(xintercept = model_ridge$lambda.min, col = "deepskyblue3") +
  geom_hline(yintercept = min(model_ridge$cvm), col = "deepskyblue3") +
  scale_x_continuous(trans = 'log10', breaks = c(0.0001,0.001,0.01, 0.1, 1, 10, 100, 1000, 10000), labels = c(0.0001,0.001,0.01, 0.1, 1, 10, 100, 1000, 10000)) + 
  scale_y_continuous(labels = scales::comma_format()) + 
  theme(legend.position = "bottom") + 
  labs(x = "Lambda", 
       y = "Cross-Validation MSE", 
       col = "Non-Zero Coefficients:", 
       title = "Ridge Regression - Lambda Selection (Using 5-Fold Cross-Validation)")
#changes lambda between 10^-4 and 4


model_ridge_best <- glmnet(train_matx,train_maty,alpha = 0,lambda = model_ridge$lambda.min)

ridge_pred <- predict(model_ridge_best, s = model_ridge$lambda.min, newx = test_matx) #????? must be 22 variable for newx
ridge_mse <- mse(ridge_pred,test_maty)


#coefficients 
ridge_coef <- predict(model_ridge_best, type = "coefficients", s = model_ridge$lambda.min)
round(ridge_coef, 3)

# The Lasso
# Fit a lasso model on the training set, with λ chosen by cross- validation. Report the test error obtained, along with the number of non-zero coefficient estimates.
set.seed(1010)
model_lasso <- cv.glmnet(train_matx,train_maty, alpha = 1, lambda = 10^seq(4, -4, length = 100), standardize = TRUE, nfolds = 5, thresh = 1e-12)

data.frame(lambda = model_lasso$lambda, 
           cv_mse = model_lasso$cvm, 
           nonzero_coeff = model_lasso$nzero) %>%
  ggplot(aes(x = lambda, y = cv_mse, col = nonzero_coeff)) + 
  geom_point() + 
  geom_line() + 
  geom_vline(xintercept = model_lasso$lambda.min, col = "deepskyblue3") +
  geom_hline(yintercept = min(model_lasso$cvm), col = "deepskyblue3") +
  scale_x_continuous(trans = 'log10', breaks = c(0.0001,0.001,0.01, 0.1, 1, 10, 100, 1000, 10000), labels = c(0.0001,0.001,0.01, 0.1, 1, 10, 100, 1000, 10000)) + 
  scale_y_continuous(labels = scales::comma_format()) + 
  theme(legend.position = "bottom") + 
  scale_color_gradient(low = "red", high = "green") +
  labs(x = "Lambda", 
       y = "Cross-Validation MSE", 
       col = "Non-Zero Coefficients:", 
       title = "Lasso - Lambda Selection (Using 5-Fold Cross-Validation)")

model_lasso_best <- glmnet(train_matx,
                           train_maty,
                           alpha = 1, 
                           lambda =  model_lasso$lambda.min)

lasso_pred <- predict(model_lasso_best, s = model_lasso$lambda.min, newx = test_matx)
lasso_mse <- mse(lasso_pred,test_maty)


#coefficients 
lasso_coef <- predict(model_lasso_best, type = "coefficients", s = model_lasso$lambda.min)
round(lasso_coef, 3)




# Principal Components Regression
# Fit a PCR model on the training set, with M chosen by cross- validation. Report the test error obtained, along with the value of M selected by cross-validation.
library(pls)
set.seed(1010)

model_pcr <- pcr(price ~ .,data = train, scale = T, validation = "CV")

model_pcr_mse <- MSEP(model_pcr, estimate = "CV")$val %>%
  reshape2::melt() %>%
  mutate(M = 0:(nrow(.)-1)) %>%
  select(M, value) %>%
  mutate(CV_MSE = value) #?????? unused argument (CV_MSE = value)

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

pcr_pred <- predict(model_pcr, test, ncomp = 19) #????? need ncomp from CV
pcr_mse <- mse(pcr_pred,test_maty)

# Partial Least Squares Regression
# Fit a PLS model on the training set, with M chosen by cross- validation. Report the test error obtained, along with the value of M selected by cross-validation.
set.seed(1010)

model_pls <- plsr(price ~ .,data = train, scale = T, validation = "CV")

model_pls_mse <- MSEP(model_pls, estimate = "CV")$val %>%
  reshape2::melt() %>%
  mutate(M = 0:(nrow(.)-1)) %>%
  select(M, value) %>%
  mutate(CV_MSE = value)

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

pls_pred <- predict(model_pls, test, ncomp = 5) #using ncomp=7 from CV gives lowest MSE
pls_mse <- mse(pls_pred,test_maty)




# Performance Comparison
SS_tot <- sum((test_maty - mean(pls_pred))^2)

data.frame(method = c("OLS", "Ridge", "Lasso", "PCR", "PLS"), 
           test_MSE = c(ols_mse, ridge_mse, lasso_mse, pcr_mse, pls_mse), 
           test_R2 = c(1 - sum((test_maty - ols_pred)^2) / SS_tot,
                       1 - sum((test_maty  - ridge_pred)^2) / SS_tot, 
                       1 - sum((test_maty  - lasso_pred)^2) / SS_tot, 
                       1 - sum((test_maty - pcr_pred)^2) / SS_tot, 
                       1 - sum((test_maty - pls_pred)^2) / SS_tot)) %>%
  arrange(test_MSE)  

library(rpart)
set.seed(1010)
m1<-rpart(price ~., data=train)
dev.new()
plot(m1)
text(m1, pretty = 0)
plotcp(m1)
scrapyhat1 = predict(m1, newdata = test)
mean((scrapyhat1-test_maty)^2)
names(m1$variable.importance)

prune(m1, cp=0.013)
m2<-prune(m1, cp=0.013)
dev.new()
plot(m2)
text(m2)
scrapyhat2 = predict(m2, newdata = test)
mean((scrapyhat2-test_maty)^2)
names(m2$variable.importance)


#variable importance??????

library(ggplot2)

######### boxplot
data = data.frame(A=(pcr_pred - test_maty)^2,
                  B=(pls_pred - test_maty)^2,
                  C=(lasso_pred - test_maty)^2,
                  D=(ridge_pred - test_maty)^2,
                  E=(ols_pred - test_maty)^2)
library(ggplot2)
boxplot(data)