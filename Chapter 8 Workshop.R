              # 8.3 Lab: Decision Trees
      # 8.3.1 Fitting Classification Trees

# The tree library is used to construct classification and regression trees.
library(tree)

# Sales is a continuous variable, and so we begin by recoding it as a binary variable. 
# use the ifelse() function to create a variable, called High, which takes on a value of Yes if the Sales variable exceeds 8, and takes on a value of No otherwise.
library(ISLR)
attach(Carseats)
High=ifelse(Sales <=8,"No","Yes")

# use the data.frame() function to merge High with the rest of the Carseats data.
Carseats =data.frame(Carseats ,High)

# use the tree() function to fit a classification tree in order to predict HighusingallvariablesbutSales
tree.carseats =tree(High~.-Sales ,Carseats)

# The summary() function lists the variables that are used as internal nodes in the tree, the number of terminal nodes, and the (training) error rate
summary(tree.carseats) #????? 

# use the plot() function to display the tree struc- ture, and the text() function to display the node labels.
# The argument pretty=0 instructs R to include the category names for any qualitative pre- dictors, rather than simply displaying a letter for each category.
plot(tree.carseats)
text(tree.carseats ,pretty =0)

tree.carseats

# we must estimate the test error rather than simply computing the training error.
# split the observations into a training set and a test set, build the tree using the training set, and evaluate its performance on the test data.
# The predict() function can be used for this purpose. In the case of a classification tree, the argument type="class" instructs R to return the actual class prediction. 
set.seed(2)
train=sample(1:nrow(Carseats), 200)
Carseats.test=Carseats[-train ,]
High.test=High[-train]
tree.carseats=tree(High~.-Sales,Carseats,subset=train) #?????
tree.pred=predict(tree.carseats,Carseats.test,type="class") #????
table(tree.pred ,High.test) #?????

# we consider whether pruning the tree might lead to improved results.
# The function cv.tree() performs cross-validation in order to determine the optimal level of tree complexity; cost complexity pruning is used in order to select a sequence of trees for consideration.
# use the argument FUN=prune.misclass in order to indicate that we want the classification error rate to guide the cross-validation and pruning process, rather than the default for the cv.tree() function, which is deviance.
# The cv.tree() function reports the number of terminal nodes of each tree con- sidered (size) as well as the corresponding error rate and the value of the cost-complexity parameter used (k, which corresponds to α
set.seed(3)
cv.carseats =cv.tree(tree.carseats ,FUN=prune.misclass )
names(cv.carseats )

cv.carseats

# plot the error rate as a function of both size and k.
par(mfrow=c(1,2))
plot(cv.carseats$size ,cv.carseats$dev ,type="b")
plot(cv.carseats$k ,cv.carseats$dev ,type="b")

# now apply the prune.misclass() function in order to prune the tree to obtain the nine-node tree.
prune.carseats=prune.misclass(tree.carseats,best=9)
plot(prune.carseats )
text(prune.carseats,pretty=0)

# we apply the predict() function.
tree.pred=predict(prune.carseats,Carseats.test,type="class")
table(tree.pred ,High.test)

# If we increase the value of best, we obtain a larger pruned tree with lower classification accuracy
prune.carseats=prune.misclass(tree.carseats,best=15)
text(prune.carseats,pretty=0)
tree.pred=predict(prune.carseats,Carseats.test,type="class")
table(tree.pred ,High.test)




    # 8.3.2 Fitting Regression Trees
# we create a training set, and fit the tree to the training data
library(MASS)
set.seed(1)
train = sample(1:nrow(Boston), nrow(Boston)/2)
tree.boston=tree(medv~.,Boston ,subset=train)
summary(tree.boston)

plot(tree.boston)
text(tree.boston ,pretty=0)

# use the cv.tree() function to see whether pruning the tree will improve performance
cv.boston=cv.tree(tree.boston)
plot(cv.boston$size ,cv.boston$dev ,type='b')

# the most complex tree is selected by cross-validation. How- ever, if we wish to prune the tree, we could do so as follows, using the prune.tree() function
prune.boston=prune.tree(tree.boston ,best=5)
plot(prune.boston)
text(prune.boston ,pretty=0)

# In keeping with the cross-validation results, we use the unpruned tree to make predictions on the test set
yhat=predict(tree.boston ,newdata=Boston[-train ,])
boston.test=Boston[-train ,"medv"]
plot(yhat,boston.test)
abline(0,1)
mean((yhat-boston.test)^2)
# the test set MSE associated with the regression tree is 25.05. The square root of the MSE is therefore around 5.005, indicating that this model leads to test predictions that are within around $5, 005 of the true median home value for the suburb



    # 8.3.3 Bagging and Random Forests
# using the randomForest package in R.
# bagging is simply a special case of a random forest with m = p.
# the randomForest() function can  be used to perform both random forests and bagging.
library(randomForest)
set.seed(1)
bag.boston=randomForest(medv~.,data=Boston,subset=train,
                          mtry=13,importance =TRUE)
bag.boston
# The argument mtry=13 indicates that all 13 predictors should be considered for each split of the tree—in other words, that bagging should be done

# How well does this bagged model perform on the test set?
yhat.bag = predict(bag.boston ,newdata=Boston[-train ,])
plot(yhat.bag, boston.test)
abline(0,1)
mean((yhat.bag-boston.test)^2)

# We could change the number of trees grown by randomForest() using the ntree argument
bag.boston=randomForest(medv~.,data=Boston,subset=train, mtry=13,ntree=25)
yhat.bag = predict(bag.boston ,newdata=Boston[-train ,])
mean((yhat.bag-boston.test)^2)
# By default, randomForest() uses p/3 variables when building a random forest of regression trees, and √p variables when building a random forest of classification trees. 

# we use mtry = 6
set.seed(1)
rf.boston=randomForest(medv~.,data=Boston,subset=train,
                         mtry=6,importance =TRUE)
yhat.rf = predict(rf.boston ,newdata=Boston[-train ,])
mean((yhat.rf-boston.test)^2)
# this indicates that random forests yielded an improvement over bagging in this case

# Using the importance() function, we can view the importance of each variable.
importance(rf.boston)

#  Plots of these importance measures can be produced using the varImpPlot() function
varImpPlot (rf.boston)
#  the wealth level of the community (lstat) and the house size (rm) are by far the two most important variables.




      # 8.3.4 Boosting
# use the gbm package, and within it the gbm() function, to fit boosted regression trees to the Boston data set
# run gbm() with the option distribution="gaussian" since this is a regression problem; if it were a bi- nary classification problem, we would use distribution="bernoulli".
# The argument n.trees=5000 indicates that we want 5000 trees, and the option interaction.depth=4 limits the depth of each tree.
library(gbm)
set.seed(1)
boost.boston=gbm(medv~.,data=Boston[train,],distribution=
                     "gaussian",n.trees=5000, interaction.depth=4)

# The summary() function produces a relative influence plot and also outputs the relative influence statistics.
summary(boost.boston)
# We see that lstat and rm are by far the most important variables.

# We can also produce partial dependence plots for these two variables.
# These plots illustrate the marginal effect of the selected variables on the response after integrating out the other variables.
dev.new()
par(mfrow=c(1,2)) 
plot(boost.boston ,i="rm") 
plot(boost.boston ,i="lstat")
# median house prices are increasing with rm and decreasing with lstat

# now use the boosted model to predict medv on the test set
yhat.boost=predict(boost.boston,newdata=Boston[-train,], n.trees=5000)
mean((yhat.boost -boston.test)^2)

# we can perform boosting with a different value of the shrinkage parameter λ. The default value is 0.001, but this is easily modified. Here we take λ = 0.2.
boost.boston=gbm(medv~.,data=Boston[train,],distribution= "gaussian",n.trees=5000,interaction.depth=4,shrinkage =0.2, verbose =F)
yhat.boost=predict(boost.boston,newdata=Boston[-train,], n.trees=5000)
mean((yhat.boost -boston.test)^2)
# using λ = 0.2 leads to a slightly lower test MSE than λ = 0.001




              # Applied

library(ISLR)
attach(Carseats)
dim(Carseats)
names(Carseats)
set.seed(1234)

train_index <- sample(1:nrow(Carseats), nrow(Carseats) / 2)
train <- Carseats[train_index, ] 
test <- Carseats[-train_index, ]

library(rpart)
library(tree)

#regression tree
dev.new()
par(mfrows = c(1,2))
tree1 <-rpart(Sales ~., data=train)
plot(tree1)
text(tree1, pretty = 0, cex = 0.7)
printcp(tree1)
summary(tree1)
test_pred <- predict(tree1, test)
mean((test_pred - test$Sales)^2)

baseline_test_pred <- mean(train$Sales)
mean((baseline_test_pred - test$Sales)^2)

#pruning with cp vales from plot
dev.new()
plotcp(tree1)
prune(tree1, cp=0.03)
tree2 <- prune(tree1, cp=0.03)
par(xpd =  TRUE)
plot(tree2)
text(tree2, pretty = 0, cex = 0.7)
summary(tree2)
test_pred2 <- predict(tree2, test)
mean((test_pred2 - test$Sales)^2)

#bagging
library(randomForest)
bagged_trees <- randomForest(y = train$Sales, 
                                   x = train[ ,-1], 
                                   mtry = ncol(train) - 1,
                                   importance = T) 
test_pred3 <- predict(bagged_trees, test)
mean((test_pred3 - test$Sales)^2)

varImpPlot(test_pred3)
importance(bagged_trees)
summary(bagged_trees)

plot(test_pred3,test$Sales)

#random forest 
test_MSE <- c()
i <- 1

for (Mtry in 1:10) {
  set.seed(1234)
  
  rf_temp <- randomForest(y = train$Sales, 
                          x = train[ ,-1], 
                          mtry = Mtry, 
                          importance = T)
  
  test_pred4 <- predict(rf_temp, test)
  
  test_MSE[i] <- mean((test_pred4 - test$Sales)^2)
  
  i <- i + 1
}

data.frame(mtry = 1:10, test_MSE = test_MSE) %>%
  mutate(min_test_MSE = as.numeric(min(test_MSE) == test_MSE)) %>%
  ggplot(aes(x = mtry, y = test_MSE)) +
  geom_line(col = "grey55") +
  geom_point(size = 2, aes(col = factor(min_test_MSE))) +
  scale_x_continuous(breaks = seq(1, 10), minor_breaks = NULL) +
  scale_color_manual(values = c("deepskyblue3", "green")) +
  theme(legend.position = "none") +
  labs(title = "Carseats Dataset - Random Forests",
       subtitle = "Selecting 'mtry' using the test MSE",
       x = "mtry",
       y = "Test MSE")

tail(test_MSE, 1)

library(dplyr)
library(tibble)

importance(rf_temp) %>%
  as.data.frame() %>%
  rownames_to_column("varname") %>%
  arrange(desc(IncNodePurity))

#boosting with 1000 trees
set.seed(1234)

pow = seq(-2,0,0.1)

lambdas = 10^pow

train.error = rep(NA, length(lambdas))
test.error = rep(NA, length(lambdas))

library(gbm)
for (i in 1:length(lambdas)) {
  
  model = gbm(Sales~., data = train,
              distribution = "gaussian",
              n.trees = 1000,
              shrinkage = lambdas[i])
  
  # predict train error
  model.preds = predict(model, train, n.trees = 1000)
  train.error[i] = mean((model.preds - train$Sales)^2)
  test.error[i] = mean((model.preds - test$Sales)^2)
  
}

#plot of corresponding lambda to MSEs
plot(lambdas, train.error, type="b", xlab = "Shrinkage", ylab = "Train MSE")
plot(lambdas, test.error, type="b", xlab = "Shrinkage", ylab = "Train MSE")

#variable importance for boosted model
boosted.model = gbm(Sales~., data = train, 
                    distribution = "gaussian", 
                    n.trees = 1000, 
                    shrinkage = lambdas[which.min(train.error)])

summary(boosted.model)

test_pred5 <- predict(boosted.model, test)
mean((test_pred5 - test$Sales)^2)

c(mean((test_pred - test$Sales)^2),mean((test_pred2 - test$Sales)^2),mean((test_pred3 - test$Sales)^2),mean((test_pred4 - test$Sales)^2),mean((test_pred5 - test$Sales)^2))





# 9
packages <- c('ISLR', 'caret', 'tidyverse', 'ggthemes', 'rpart', 'rpart.plot', 
              'knitr', 'kableExtra')
sapply(packages, require, character.only = TRUE)
data(OJ)
names(OJ)

# (a) Create a training set containing a random sample of 800 obser- vations, and a test set containing the remaining observations.
set.seed(1)
inTrain <- createDataPartition(OJ$Purchase, p = 800/1070, list = FALSE)
training <- OJ[inTrain,]
testing <- OJ[-inTrain,]

# (b) Fit a tree to the training data, with Purchase as the response and the other variables except for Buy as predictors. 
# Use the summary() function to produce summary statistics about the tree, and describe the results obtained.
# What is the training error rate? How many terminal nodes does the tree have?
tree_model <- tree(Purchase ~ ., training)
summary(tree_model)

# (c) Type in the name of the tree object in order to get a detailed text output. 
# Pick one of the terminal nodes, and interpret the information displayed.
tree_model
library(caret) #dummyvars
library(glmnet) # for ridge regression & lasso
library(dplyr)

# We calculate can the deviance of node 11 only
# ??????
train %>%
  filter(LoyalCH < 0.5036, 
         LoyalCH > 0.142213, 
         PriceDiff > 0.235) %>%
  select(Purchase) %>% 
  table()

-2 * (68 * log(68/118) + 50 * log(50/118))

# (d) Create a plot of the tree, and interpret the results
dev.new()
plot(tree_model)
text(tree_model, pretty = 0, cex = 0.7)

# (e) Predict the response on the test data, and produce a confusion matrix comparing the test labels to the predicted test labels. What is the test error rate?
test_pred <- predict(tree_model, testing, type = "class")
table(test_pred, test_actual = testing$Purchase)

# The test error rate
1 - mean(test_pred == testing$Purchase)

# CH was the most common orange juice in train so, for comparison, a baseline classifier (that predicted CH for all observations in test) would have the following error rate
1 - mean(testing$Purchase == "CH")

# (f) Apply the cv.tree() function to the training set in order to determine the optimal tree size.
set.seed(2)
cv_tree_model <- cv.tree(tree_model, K = 10, FUN = prune.misclass)
cv_tree_model

# (g) Produce a plot with tree size on the x-axis and cross-validated classification error rate on the y-axis.
# ??????
data.frame(size = cv_tree_model$size, CV_Error = cv_tree_model$dev / nrow(train)) %>%
  mutate(min_CV_Error = as.numeric(min(CV_Error) == CV_Error)) %>%
  ggplot(aes(x = size, y = CV_Error)) +
  geom_line(col = "grey55") +
  geom_point(size = 2, aes(col = factor(min_CV_Error))) +
  scale_x_continuous(breaks = seq(1, 7), minor_breaks = NULL) +
  scale_y_continuous(labels = scales::percent_format()) +
  scale_color_manual(values = c("deepskyblue3", "green")) +
  theme(legend.position = "none") +
  labs(title = "OJ Dataset - Classification Tree",
       subtitle = "Selecting tree 'size' (# of terminal nodes) using cross-validation",
       x = "Tree Size",
       y = "CV Error")


# (h) Which tree size corresponds to the lowest cross-validated classi- fication error rate?
      # Of the sequence of trees generated, trees of sizes 4 and 7 have the same cross-validation error. It makes sense to select the more parsimonious model here with 4 terminal nodes

# (i) Produce a pruned tree corresponding to the optimal tree size obtained using cross-validation. If cross-validation does not lead to selection of a pruned tree, then create a pruned tree with five terminal nodes.
pruned_tree_model <- prune.tree(tree_model, best = 4)
pruned_tree_model

# (j) Compare the training error rates between the pruned and un- pruned trees. Which is higher?
#  training error for the unpruned tree (7 terminal nodes)
mean(predict(tree_model, type = "class") != training$Purchase)

#  the pruned tree (4 terminal nodes)
mean(predict(pruned_tree_model, type = "class") != training$Purchase)

# (k) Compare the test error rates between the pruned and unpruned trees. Which is higher?
# The test error for the unpruned tree
mean(predict(tree_model, type = "class", newdata = testing) != testing$Purchase)

# The same for the pruned tree
mean(predict(pruned_tree_model, type = "class", newdata = testing) != testing$Purchase)
