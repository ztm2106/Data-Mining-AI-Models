      #Applied 
library(ISLR)
attach(Auto)

#ifelse statement to Create a binary variable
Auto$mpg=ifelse(Auto$mpg>median(Auto$mpg),1,0)
table(Auto$mpg)

library(e1071)
#Fit a support vector classifier to the data with various values of cost, in order to predict whether a car gets high or low gas mileage. 
costs=data.frame(cost=seq(0.05,100,length.out = 15))               # tuning grid for the cost parameter.
svm.tune=tune(svm,mpg~.,data=Auto,ranges=costs,kernel='linear')     # 10-fold cross validation.
svm.tune
dev.new()
plot(svm.tune, data=Auto, horsepower ~displacement)

plot(svm.tune$performance[,c(1,2)],type='l')
#the cross-validation errors associated with different values of this parameter.
# cost of about 5 or 10 has the most reduction in the miss classification error

# SVMs with radial and polynomial basis kernels, with different values of gamma and degree and cost.
#degrees for poly
params=data.frame(cost=seq(0.05,100,length.out = 5),degree=seq(1,100,length.out = 5))
svm.poly=tune(svm,mpg~.,data=Auto,ranges=params,kernel='polynomial')
svm.poly

#gamma for radial
params=data.frame(cost=seq(0.05,100,length.out = 5),gamma=seq(0.1,100,length.out = 5))
svm.radial=tune(svm,mpg~.,data=Auto,ranges=params,kernel='radial')
svm.radial


plot(svm.tune, data=Auto)
plot(svm.poly, data=Auto)
plot(svm.radial, data=Auto)


