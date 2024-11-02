#````````````Beginner Math on R ````````````
# use x as a variable to hold the list of number with c
x <- c(2, 3, 5, 9, 10, 10, 10, 10)

#square root the list that is saves to x and save it to new variable y
y <- sqrt(x)

# take the log of the initial list and save save to new variable z
z <- log(x)

# take the inverse of the set and save to a
a <- 1/x

#reflection transformation

#transforming to normality
# to backtransform

  
ex.mean <- mean(x)
ex.variance <- var(x)
ex.mode <- mode(x)
  

#````````Covariance``````````````
# set b and c variables to water (g) and weight (%) 
b <- c(10, 20, 25, 30 , 35, 45)
c <- c(13, 16, 14, 18, 16, 19)

# plotted the points of b and c where b is the independent variable
plot(b,c)

# use cov() to find the covariance
ex.cov <- cov(b,c)

#````````Correlation``````````
# used cor.test() to test the correlations and gives of stats data
# P value, t stat, df,cor, conf.int,  method, type of test 
ex.correl <- cor.test(b,c)

#```````Z-Score```````
#explains the standard deviation from the mean of set of each data point
ex.zscore <- scale(x)

#``````` Area Under the Standard Curve ``````````````
#from the left
ex.pnorm.left <- pnorm(2)

#from the right you have to subtract 1
ex.pnorm.right <- 1 - pnorm(2)

#has to equal 1
ex.pnorm.left + ex.pnorm.right

#``````````````Finding Critical Val "Z* alpha//2 ````````````````
#have toe mean, sd, and p value
#Conf Level 95%
ex.zcritical <- qnorm(p = 0.025, mean = 0, sd = 1)

#``````````````Student t distribution (t* alpha/2)``````````````
#another critical value
ex.studenttdistribution <- qt(p=0.025,df=9)

#to get positive take the absolute
abs(ex.studenttdistribution)

#```````````````One-Sample t-test`````````````
#census on entire pop.
#no pop. stand. dev. -- σ or pop. mean -- µ
#get t critical val 
ex.tcritical1left <- qt(p=0.025,df=39)
ex.tcritical1right <- qt(p=0.975,df=39)

#get p value
#the area to the left of a given value x in the Student t distribution
ex.pval1left <- pt(q=-1.501,df=39)
#subsract 1 for the right
ex.pval1right <- 1-pt(q=1.501,df=39)

#``````````````````Two Sample t-test```````````````
#find the t critical vals
ex.tcritical2left <- qt(p=0.025,df=185)
ex.tcritical2right <- qt(p=0.975,df=185)

# find p vals
ex.pval2left <- pt(q=-0.676,df=185)
ex.pval2right <- 1-pt(q=-0.676,df=185)

#`````````````````Paired t-test ```````````````
#can use table and right side t dis.
# t critcals
qt(p=0.025,df=4)
qt(p=0.975,df=4)

#p vals
pt(q=-0.186,df=4)
1-pt(q=-0.186,df=4)

#````````````F Test```````````
#two pop with diff. variances /sigma^2
#always positive
#get F critical
qf(0.975,df1=39,df2=39)

#get p val
#only need the right
#pf gives a probability, so it cannot be greater than one.
(1-pf(1.9729,df1=39,df2=39))*2

#``````````````Significance Test and Correlation````````````````
#significance
cor(x,y, method = c("pearson", "kendall", "spearman"))

#correlation
cor.test(x,y, alternative = c("two.sided", "less", "greater"), method = c("pearson", "kendall", "spearman"), conf.level = 0.95)





