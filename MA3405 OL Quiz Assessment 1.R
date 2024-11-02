#Review Quiz
#Q1
# A group of athletes all run a 100m sprint. The average of their times is 12.3s, with a standard deviation of 1.1s. 
#If an athlete is randomly selected from the group, what is the probability that their time will be greater than 10.9s?

#if higher/more than
round(1-pnorm(q=10.9,mean=12.3,sd=1.1),2)


#Q2 
# A group of athletes all run a 100m sprint. The average of their times is 12.3s, with a standard deviation of 1.1s.
#If 8 athletes are randomly selected from the group, what is the probability that the average of their times will be less than 11.4s?

#if lower/less than 
round(pnorm(q=10.9,mean=12.3,sd=1.1),2)

#Q3
#One way to eliminate bias is to select simple random samples.

#True

#Q4
#In an experiment...

#we apply some treatment, then proceed to observe its effects on the subjects being studied.

#Q5
#In a frequency histogram that is right skewed:

#the mode < median < mean

#Q6
#If a sample does not exhibit significant skewness or kurtosis, then:

#the median = the mode = the mean

#Q7
#The probability of an event...

#is the proportion of times the event would occur if we repeated a random trial over and over again under the same conditions


                              
#Q8
#Scores on a university exam are Normally distributed with a mean of 78 and a standard deviation of 8. The professor teaching 
#the class declares that a score of 70 or higher is required for a grade of at least “C.” Using R, the area under the standard Normal 
#curve corresponding to –0.5 < Z < 1.2 is

#use interval piece to get both observed values by using Z-score eq
pnorm(q=74,mean=78,sd=8)-pnorm(q=87.6,mean=78,sd=8)

#Q9
#F-procedures for comparing variances of populations are...

#not robust/ not strong

#NOTE: a test is robust if it still provides insight into a problem despite having its assumptions altered or violated


#Q10
#What would happen if instead of using an ANOVA to compare 10 groups, you performed multiple t-tests?

#Making multiple comparisons with a t-test increases the probability of making a Type I error

#NOTE:
#A type 1 error is also known as a false positive and occurs when a researcher incorrectly rejects a true null hypothesis.
#A type II error is also known as a false negative and occurs when a researcher fails to reject a null hypothesis which is really false.

#Q11 !!!!!!!
# A group of enterprising university students decide to examine the distribution of beer prices between various pubs and clubs. They visit 20 establishments in Townsville 
# and find that the average price of a schooner of the cheapest beer on tap is $6.37, with a standard deviation of $0.27. 
# Based on this sample, what range of values would have a 99% chance of encompassing the true population mean price? 
# Assume that beer prices are normally distributed and the pubs and clubs represent a simple random sample. 

# since 99% confidence we use .01/2 = 0.005
# df (degrees of freedom) = n-1
#get tdistribution, put that to ME eq, than +- ME from mean
tdist <- qt(p=0.005,df=19)
ME <- -tdist*(0.27/(sqrt(20)))
 round(6.37-ME,2)
 round(6.37+ME,2)
 
 
#Q12
# A survey of 200 university students found that the average number of hours per week that students working in casual jobs was 7.10. If the standard deviation for hours 
# worked per week was 5 hours, what is the 95% confidence interval based on the sample of 200 students?

# since 95% confidence we use .05/2 = 0.025
# df (degrees of freedom) = n-1
#get tdistribution, put that to ME eq, than +- ME from mean
tdist <- qt(p=0.025,df=199)
ME <- -tdist*(5/(sqrt(200)))
round(7.10-ME,2)
round(7.10+ME,2)


#Q13
# Which of the following graphical techniques is suitable for data on a nominal scale?

# Bar graph and pie diagram

#Q14

 
qnorm(p=0.005,mean=6.37,sd=0.27)






# ```````OL Quiz 2```````


#Q2 Using R commands, how would you calculate the t-value for which 95% of the data lies below for a 
# t-distribution with 14 degrees of freedom?
qt(.95,14)


# Q3 Scores on the SAT verbal test in recent years follow approximately the N(515, 109) distribution. 
# How high must a student score in order to place in the top 5% of all students taking the SAT?

qnorm(p = 0.05, mean = 515, sd = 109)


#Q10
qf(0.95,df1=9,df2=9)


1-pnorm(0.6)
scale(0.2742531)

qnorm(p = 0.95, mean = 249, sd = 30)


qf(0.95,df1=20,df2=20)

qt(p=0.95,df=9)


qnorm(p = 0.74, mean = 3.3, sd = .4)

pnorm(q=65,mean=78,sd=8)

pnorm(q=70,mean=78,sd=8)

1-pnorm(q=145,mean=138.9,sd=8.6)
1-pnorm(q=145,mean=138.9,sd=8.6)
pnorm(q=120,mean=3.3,sd=0.4)

qt(p=0.95,df=24)
qt(p=0.05,df=120)

pnorm(q=80,mean=100,sd=15)

qnorm(p = 0.9, mean = 23.5, sd = 7.8)

qnorm(p = 0.05, mean = 23.5, sd = 7.8)

pnorm(q=80,mean=100,sd=15)-pnorm(q=120,mean=100,sd=15)

qnorm(p=0.05,mean=7.1,sd=5)-qnorm(p=0.95,mean=7.1,sd=5)

pnorm(q=138.9,mean=145,sd=8.6)

pnorm(-2) + (1-pnorm(2))  

pnorm(q=3.0,mean=3.3,sd=0.4)

1-pnorm(q=11.4,mean=12.3,sd=0.3889)

scale((1-pt(q=2.863,df=34))*2)

pnorm(-2) + (1 -pnorm(2))

qt(p=0.005,df=32)


1-pnorm(q=3.9,mean=3.3,sd=0.4)
((7.8/sqrt(33)))+23.5


#~~~~~Z stat
qnorm(p=,mean=,sd=)
qnorm(p=0.025,mean=6.37,sd=0.54)
qnorm(p=0.95,mean=23.5,sd=1.35)

1-pnorm(q=110,mean=100,sd=15)
pnorm(q=70,mean=78,sd=8)
pnorm(q=74,mean=78,sd=8)-pnorm(q=87.6,mean=78,sd=8)
pnorm(q=460,mean=515,sd=109)-pnorm(q=550,mean=515,sd=109)
1-pnorm(q=3.9,mean=3.3,sd=0.4)
1-pnorm(q=120,mean=110,sd=4)
1-pnorm(q=10.9,mean=12.3,sd=1.1)
1-pnorm(q=145,mean=138.9,sd=8.6)






#`````T stat
qt(p=,df=)

1-qt(p=0.975,df=9)
qt(p=0.99,df=32)



#``````Prob.
#add th two probs for two tailed
pt(q=,df=)
1-pt(q=2.99795,df=7)

#````` F stat
qf(p=,df1=,df2=)

qf(p=0.975,df1=9,df2=9)

#prob.
(1-pf(q=,df1=,df2=39))*2
(1-pf(q=13.29,df1=1,df2=28))*2

x <- c(95,38,221,122,258,237,233)
median(x)

x1 <- c(11.8, 11.5, 18.9, 18.5, 12.5, 15.9, 16.8, 15.8, 14.6, 17.4)
sd(x1)


help(pt)
help(qt)
help(lm)
