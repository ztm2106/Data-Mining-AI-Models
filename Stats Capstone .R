rm(list = ls())
options(max.print=10000)
#IMPORTING CSV FILE
scrap_price = read.csv(file.choose())

library(pls)
library(glmnet) # for ridge regression & lasso
library(dplyr)
library(ggplot2)

dim(scrap_price)

      #Changing all categorical variable into numerical variable to begin to look at correlation of the data set as a whole
#Fueltypes: Gas = 2 & Diesel =1
scrap_price$fueltypes<- as.factor(scrap_price$fueltypes)
scrap_price$fueltypes<- as.numeric(scrap_price$fueltypes) 
max(scrap_price$fueltypes)

#Aspirations: Standard = 1 & Turbo = 2
scrap_price$aspiration<- as.factor(scrap_price$aspiration)
scrap_price$aspiration<- as.numeric(scrap_price$aspiration)
max(scrap_price$aspiration)

#Number of Doors: Two Door = 2 Four Door = 1
scrap_price$doornumbers<- as.factor(scrap_price$doornumbers)
scrap_price$doornumbers<- as.numeric(scrap_price$doornumbers)
max(scrap_price$doornumbers)

#Car Body: Convertible = 1 Sedan = 4 Hatchback = 3 Hardtop = 2 Wagon = 5
scrap_price$carbody<-data.matrix(data.frame(unclass(scrap_price$carbody))) 
max(scrap_price$carbody)

#Drive wheels: rwd = 3 fwd = 2 4wd = 1
scrap_price$drivewheels<-data.matrix(data.frame(unclass(scrap_price$drivewheels))) 
max(scrap_price$drivewheels)

#Engine Location: Front = 1
scrap_price$enginelocation<- as.factor(scrap_price$enginelocation)
scrap_price$enginelocation<- as.numeric(scrap_price$enginelocation)

#Engine Type: dohc = 1 ohcv = 6 I = 3 ohc = 4 rotor = 7 ohcf = 5 dohcv = 2
scrap_price$enginetype<-data.matrix(data.frame(unclass(scrap_price$enginetype))) 
max(scrap_price$enginetype)

#Cylinders: TwoCyl = 7 ThreeCyl = 5  FourCyl = 3 FiveCyl = 2 SixCyl = 4 EightCyl = 1 TwelveCyl =6
scrap_price$cylindernumber<-data.matrix(data.frame(unclass(scrap_price$cylindernumber))) 
max(scrap_price$cylindernumber)

# Fuel System: mpfi = 6 idi = 4 4bbl = 3 2bbl = 2 1bbl = 1 spfi = 8  spdi = 7 mfi = 5
scrap_price$fuelsystem<-data.matrix(data.frame(unclass(scrap_price$fuelsystem))) 
max(scrap_price$fuelsystem)

#Nammes of each car
scrap_price$name<-data.matrix(data.frame(unclass(scrap_price$name))) 
max(scrap_price$name)

View(scrap_price)
summary(scrap_price)
names(scrap_price)
pairs(scrap_price)
cor(scrap_price)

mse <- function(x,y) {mean((x-y)^2)}

              #Splitting test and training sets of data
set.seed(1010)

scrap_price$price = scrap_price$price/100
sample <- sample(c(TRUE, FALSE), nrow(scrap_price), replace=TRUE, prob=c(0.7,0.3))

train <- scrap_price[sample, ]
nrow(train)/nrow(scrap_price)
nrow(train)

test <- scrap_price[!sample, ]
nrow(test)/nrow(scrap_price)
nrow(test)

train_matx = model.matrix(price~. -1, data = train)
train_maty= train$price
test_matx = model.matrix(price~. -1, data = test)
test_maty = test$price

# OLS Regression
# Fit a linear model using least squares on the training set, and report the test error obtained.
model_linear <- lm(price~ ., data = train)
summary(model_linear)              
car::vif(model_linear)


            #Splitting test and training sets of data with out some variables
set.seed(1010)

sample2 <- sample(c(TRUE, FALSE), nrow(scrap_price), replace=TRUE, prob=c(0.7,0.3))

train2  <- scrap_price[sample2, -c(4,10,11,14,21,22,24,25)]
nrow(train2)/nrow(scrap_price)
nrow(train2)

test2 <- scrap_price[!sample2, -c(4,10,11,14,21,22,24,25)]
nrow(test2)/nrow(scrap_price)
nrow(test2)

train_matx2 = model.matrix(price~. -1, data = train2)
train_maty2 = train2$price
test_matx2 = model.matrix(price~. -1, data = test2)
test_maty2 = test2$price

# OLS Regression
# Fit a linear model using least squares on the training set, and report the test error obtained.
model_linear2 <- lm(price~ ., data = train2)
summary(model_linear2)              

#use MSE as the test error metric
ols_pred2 <- predict(model_linear2, test2)
ols_mse2 <- mse(ols_pred2,test2$price)


# Ridge Regression
# Fit a ridge regression model on the training set, with λ chosen by cross-validation. Report the test error obtained.
set.seed(1010)

#changes lambda between 10^-4 and 4
model_ridge2 <- cv.glmnet(train_matx2,train_maty2,alpha = 0,lambda = 10^seq(4,-4, length = 100),standardize = TRUE,nfolds = 5)

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


model_ridge_best2 <- glmnet(train_matx2,train_maty2,alpha = 0,lambda = model_ridge2$lambda.min)
ridge_pred2 <- predict(model_ridge_best2, s = model_ridge2$lambda.min, newx = test_matx2)
ridge_mse2 <- mse(ridge_pred2,test_maty2)

# The Lasso
# Fit a lasso model on the training set, with λ chosen by cross- validation. Report the test error obtained, along with the number of non-zero coefficient estimates.
set.seed(1010)
model_lasso2 <- cv.glmnet(train_matx2,train_maty2, alpha = 1, lambda = 10^seq(4, -4, length = 100), standardize = TRUE, nfolds = 5, thresh = 1e-12)

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

model_lasso_best2 <- glmnet(train_matx2,
                           train_maty2,
                           alpha = 1, 
                           lambda =  model_lasso2$lambda.min)

lasso_pred2 <- predict(model_lasso_best2, s = model_lasso2$lambda.min, newx = test_matx2)
lasso_mse2 <- mse(lasso_pred2,test_maty2)

# Principal Components Regression
# Fit a PCR model on the training set, with M chosen by cross- validation. Report the test error obtained, along with the value of M selected by cross-validation.

set.seed(1010)

model_pcr2 <- pcr(price ~ .,data = train2, scale = T, validation = "CV")

model_pcr_mse2 <- MSEP(model_pcr2, estimate = "CV")$val %>%
  reshape2::melt() %>%
  mutate(M = 0:(nrow(.)-1)) %>%
  select(M, value) %>%
  mutate(CV_MSE = value) 

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

pcr_pred2 <- predict(model_pcr2, test2, ncomp = 17)
pcr_mse2 <- mse(pcr_pred2,test_maty2)

# Partial Least Squares Regression
# Fit a PLS model on the training set, with M chosen by cross- validation. Report the test error obtained, along with the value of M selected by cross-validation.
set.seed(1010)

model_pls2 <- plsr(price ~ .,data = train2, scale = T, validation = "CV")

model_pls_mse2 <- MSEP(model_pls2, estimate = "CV")$val %>%
  reshape2::melt() %>%
  mutate(M = 0:(nrow(.)-1)) %>%
  select(M, value) %>%
  mutate(CV_MSE = value)

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

pls_pred2 <- predict(model_pls2, test2, ncomp = 15) 
pls_mse2 <- mse(pls_pred2,test_maty2)

# Performance Comparison
SS_tot2 <- sum((test_maty2 - mean(pls_pred2))^2)

data.frame(method = c("OLS", "Ridge", "Lasso", "PCR", "PLS"), 
           test_MSE = c(ols_mse2, ridge_mse2, lasso_mse2, pcr_mse2, pls_mse2), 
           test_R2 = c(1 - sum((test_maty2 - ols_pred2)^2) / SS_tot2,
                       1 - sum((test_maty2  - ridge_pred2)^2) / SS_tot2, 
                       1 - sum((test_maty2  - lasso_pred2)^2) / SS_tot2, 
                       1 - sum((test_maty2 - pcr_pred2)^2) / SS_tot2, 
                       1 - sum((test_maty2 - pls_pred2)^2) / SS_tot2)) %>%
  arrange(test_MSE)


######### boxplot
data2 = data.frame(pcr=(pcr_pred2 - test_maty2)^2,
                  pls=(pls_pred2 - test_maty2)^2,
                  lasso=(lasso_pred2 - test_maty2)^2,
                  rigde=(ridge_pred2 - test_maty2)^2,
                  ols=(ols_pred2 - test_maty2)^2)
names=c("pcr","pls","lasso","ridge","ols")

boxplot(data2, names = names, xlab = "Regression Models", ylab= "MSE", main = "Performance Comparison Using MSE")





















          


