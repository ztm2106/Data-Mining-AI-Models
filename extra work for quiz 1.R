# For a one-way ANOVA, if the value of the test statistic is F=0.9, the result is statistically significant
  #never
# Which of the following is a property of the sampling distribution of ﻿top enclose y﻿?
  # The mean of the sampling distribution is µ, the population mean.
# The distribution describing the outcome of probabilities for all outcomes from tossing a coin 5 times is the:
  # Binomial distribution
# The Central Limit Theory states that
  # the sum or mean of a large number of measurements randomly sampled from a non-normal population is approximately normally distributed
# In a Pareto chart the
  # bars represent individual frequency in descending order and the line represents cumulative percentage of the whole.
# In a study, subjects are randomly assigned to one of three groups: control, experimental A, or experimental B. After treatment, the mean scores for the three groups are compared. The appropriate statistical test for comparing these means is:
  #the analysis of variance 
# A fisheries researcher wishes to test for a difference in mean weights of a single species of fish caught by fishermen in three different lakes in Nova Scotia. The significance level for the test will be 0.05.
  # 5.39
# Which of the following are all measures of dispersion?
  # Variance, range, and inter-quartile range
# Which of the following are the two most commonly used measures of variability?
  # Variance and standard deviation
# The latter percentage is an example of a ______, while the former is an example of a _______. 
  # Statistic, Parameter
# The number of hours a light bulb burns before failing varies from bulb to bulb. The distribution of burn out times is strongly skewed to the right. The central limit theorem says that
  # The average burnout time of a large number of bulbs has a distribution that is close to normal.
# Which of the following are least affected if an outlier is added to your data?
  # The median and interquartile range
# The sampling distribution is the
  # probability distribution of values for an estimate that we might obtain when we sample a population
# What is the function of a post-hoc test in an ANOVA?
  # Describe those groups that have reliable differences between group means.
# When the k population means are truly different from each other, it is likely that the average residual deviation
  # is relatively small compared to the average treatment deviations
# What would happen if instead of using an ANOVA to compare 10 groups, you performed multiple t-tests?
  # Making multiple comparisons with a t-test increases the probability of making a Type I error




#Q3.4
x <- c(5.6,5.2,4.6,5.7,6.4)
mean(x)
sd(x)
var(x)

quantile(x)

Q4.2
#z scores
pnorm(1.54)
pnorm(-2.67)
pnorm(1.96)-pnorm(-1.96)

qnorm(0.05)
qnorm(0.5)
qnorm(0.1)

pnorm(40,30,4)
qnorm(0.95,33,0.7)
qnorm(0.90,100,15)


1-pnorm(11.4,12.3,0.3889)
qnorm(p=0.95,mean=16.2,sd=3.6)

1.657759*(2*(3.6/sqrt(120)))
16.2+1.08959


Q6.4
#t states 
qt(p=0.975,df=9)
qt(p=0.975,df=44)
qt(p=0.975,df=22)
qt(p=0.995,df=27)
qt(p=0.995,df=13)
qt(p=0.95,df=6)
qt(p=0.90,df=18)
qt(p=0.95,df=119)


d <- c(95, 38, 221, 122, 258, 237, 233)
median(d)

s<-c(131, 119, 138, 125, 129, 126, 131, 132, 126, 128, 128, 131)

smean<-mean(s)
tstat<-qt(p=0.975,df=11)
sd<-sd(s)

ME<-tstat*(sd/sqrt(12))
smean-ME
smean+ME


tstat<-qt(p=0.975,df=119)
sd1<-3.6

ME<-tstat*(sd1/sqrt(120))
16.2-ME
16.2+ME

pnorm(-2) + (1 -pnorm(2)) 

qt(p=0.015,df=7)

sd<-sd(c(11.8, 11.5, 18.9, 18.5, 12.5, 15.9, 16.8, 15.8, 14.6, 17.4))
sd

1-pnorm(600,515,109)

pnorm(80,100,15)-pnorm(120,100,15)

1-pnorm(145,138.9,2.592998)

qt(0.995,35)

d<-c(0.95, 1.02, 1.01, 0.98)
dm<-mean(d)
sdm<-sd(d)

qnorm(0.025,dm,sdm)

qt(0.975, 9)

16-qt(0.96,119) * (3.6/sqrt(120))


mean(s)
sd(s)

mean(s)+qt(0.975,11) * sd(s)/sqrt(12)

pf(2.33,43,43)

pnorm(74,78,8)-pnorm(87.6,78,8)

pnorm(10.9,12.3,1.1)
qnorm(.90,100,15)
qnorm(.95,33,0.7)
qt(.995,12)
qt(.995,32)
qt(.995,32)*7.8/sqrt(33)+23.4


pnorm(62,78,8)
qnorm(.995,6.37,0.27)

qf(0.975,8,7)

1-pnorm(600,515,109)

pnorm(145,138.9,8.6)

qt(0.975,19)

pnorm(29000,29321,212)

pnorm(11.4,12.3,0.38890)

1-pnorm(10.9,12.3,1.1)
pnorm(145,138.9,2.5929)

qt(.975,9)

x=c(7,7,9,33,05,05,10,06,09,16)
mean(x)
sd(x)

y=c(0.95, 1.02, 1.01, 0.98)
mean(y)
sd(y)
qt(.975,3)
mean(y)
((sd(y)/sqrt(4))*qt(.975,3))+mean(y)


1.1/sqrt(8)
1-pnorm(11.4,12.3,0.388)


qt(.975,9)

pnorm(130,100,15)

qnorm(.87,3.3,0.03)
qt(.875,162)

g=c(95, 38, 221, 122, 258, 237, 233)
median(g)
sd(g)

d=c(15.8, 15.5, 16.4, 16.6, 15.7)


qt(.975,9)
30/sqrt(10)
pnorm(87.6,78,8)-pnorm(74,78,8)

qt(.87, 162)
qnorm(.25,3.3,0.4)

qf(0.95,9,9)


qt(.999,9)

pnorm(29000,2120,2.12)

g=c(185, 199, 158, 166, 182, 175, 130, 196, 160, 181, 184, 174, 188, 166, 175)
mean(g)
median(g)

f=c(3, 4, 5)
var(f)
















