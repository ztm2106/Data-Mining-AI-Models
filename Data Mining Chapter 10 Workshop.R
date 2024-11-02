#````````````Clustering ```````````


#``````10.5.1 K-means Clustering
# kmeans() performs the K-mean clustering
#SIMPLE SIMULATE EX. ~ truly 2 clusters 
#first 25 obs. has mean shift relative to the next 25 obs.

set.seed(2)
x=matrix(rnorm(50*2), ncol=2)
x[1:25,1]=x[1:25,1]+3
x[1:25,2]=x[1:25,2]-4

#perform K-means clustering with K=2
km.out=kmeans(x,2,nstart=20)

#cluster assignments of 50 obs are in km.out$cluster
km.out$cluster

#plot data with each obs. colored by cluster
#two dimensional because of 2 clusters
#added title
dev.new()
plot(x, col=(km.out$cluster+1), main="K-Means Clustering Results with K=2", xlab="", ylab="", pch=20, cex=2)



#for real data we do not know the true # of clusters
#perform K-means clustering with K=3
set.seed(4)
km.out=kmeans(x,3,nstart=20)
km.out

#plot the data
plot(x, col=(km.out$cluster+1), main="K-Means Clustering Results with K=3", xlab="", ylab="", pch=20, cex=2)




#use kmean() with multi. initial cluster assignment, use nstart
#nstart > 1 then K means clustering will use multiple random assignments

#comparing nstart=1 and nstart=20
#km.out$tot.withinss is the total within-cluster sum of squares
#within-cluster sum of squares are contained in km.out$withinss
#we seek to minimize by performing K-Means clustering
# strongly recommend using nstart=20 or 50 if not an undesirable local optimum may be obtained
#important to set a random seed for initial cluster assignments in step 1 can be replicated and K-means output is fully reproduced

set.seed(3)

#nstart=1, higher tot.withinss
km.out=kmeans(x,3,nstart=1)
km.out$tot.withinss

#nstart=20, lower tot.withinss
km.out=kmeans(x,3,nstart=20)
km.out$tot.withinss







#``````10.5.2 Hierarchical Clustering

# hclust() function implements hierarchical clustering
#dist() function is used to compute the 50 × 50 inter-observation Euclidean distance matrix
#complete linkage
hc.complete=hclust(dist(x), method="complete")

#average linkage
hc.average=hclust(dist(x), method="average")

#single linkage
hc.single=hclust(dist(x), method="single")

# plot() is for dendrograms
#each observation 1-3
dev.new()
par(mfrow=c(1,3))
plot(hc.complete,main="Complete Linkage", xlab="", sub="", cex=.9)
plot(hc.average, main="Average Linkage", xlab="", sub="", cex=.9)
plot(hc.single, main="Single Linkage", xlab="", sub="", cex=.9)

# cutree() to determine the cluster labels for each observation associated with a given cut of the dendrogram 
cutree(hc.complete, 2) # numbers come out differently???
cutree(hc.average, 2)
cutree(hc.single, 2)

# more sensible answer is obtained when four clusters are selected, although there are still two singletons
cutree(hc.single, 4)

# To scale the variables before performing hierarchical clustering
xsc=scale(x)
dev.new()
plot(hclust(dist(xsc), method="complete"), main="Hierarchical Clustering with Scaled Features")

# Correlation-based distance use as.dist() so hclust() function recognizes as a distance matrix
x=matrix(rnorm(30*3), ncol=3)
dd=as.dist(1-cor(t(x)))
dev.new()
plot(hclust(dd, method="complete"), main="Complete Linkage with Correlation-Based Distance", xlab="", sub="")


#`````````Exercises```````````

#2 Hierarchical

d = as.dist(matrix(c(0.0, 0.3, 0.4, 0.7,                      
                     0.3, 0.0, 0.5, 0.8,                     
                     0.4, 0.5, 0.0, 0.45,                     
                     0.7, 0.8, 0.45, 0.0), nrow = 4)) 

#a
hc2.complete=hclust(d, method = "complete")
dev.new()
plot(hc2.complete)
#b
hc2.single=hclust(d, method = "single")

par(mfrow=c(1,2))
plot(hc2.complete,main="Complete Linkage", xlab="", sub="", cex=.9)
plot(hc2.single, main="Single Linkage", xlab="", sub="", cex=.9)

#c
cutree(hc2.complete, 2)
# (1,2) and (3,4)



#d
cutree(hc2.single, 2)
#((1,2),3) and (4)

#e
plot(hclust(d, method = "complete"), labels = c(2,1,4,3))
#two or more of the leaves are repositioned, but for which the meaning of the dendrogram is the same



#3 K-means clustering

#a Plot the observations of X1 and X2 in tuple and cbind() to combine the data 
x<-cbind(c(1, 1, 0, 5, 6, 4), c(4, 3, 4, 1, 2, 0))
dev.new()
plot(x[,1], x[,2])

#b Randomly assign a cluster label to each observation. Report the cluster labels for each observation
set.seed(1)
labels <- sample(2, nrow(x), replace = T)
labels
# [1] 1 2 1 1 2 1

plot(x[, 1], x[, 2], col = (labels + 1), pch = 20, cex = 2)

#c  Compute the centroid for each cluster
centroid1 <- c(mean(x[labels == 1, 1]), mean(x[labels == 1, 2]))
centroid2 <- c(mean(x[labels == 2, 1]), mean(x[labels == 2, 2]))
plot(x[,1], x[,2], col=(labels + 1), pch = 20, cex = 2)
points(centroid1[1], centroid1[2], col = 2, pch = 4)
points(centroid2[1], centroid2[2], col = 3, pch = 4)

#d Assign each observation to the centroid to which it is closest, in terms of Euclidean distance. Report thecluster labels for each observation.
labels <- c(1, 1, 1, 2, 2, 2)
plot(x[, 1], x[, 2], col = (labels + 1), pch = 20, cex = 2)
points(centroid1[1], centroid1[2], col = 2, pch = 4)
points(centroid2[1], centroid2[2], col = 3, pch = 4)

#e  Repeat (c) and (d) until the answers obtained stop changing.
centroid1 <- c(mean(x[labels == 1, 1]), mean(x[labels == 1, 2]))
centroid2 <- c(mean(x[labels == 2, 1]), mean(x[labels == 2, 2]))
plot(x[,1], x[,2], col=(labels + 1), pch = 20, cex = 2)
points(centroid1[1], centroid1[2], col = 2, pch = 4)
points(centroid2[2], centroid2[2], col = 3, pch = 4)

# f. In your plot from (a), color the observations according to the clusters labels obtained.
plot(x[, 1], x[, 2], col=(labels + 1), pch = 20, cex = 2)


#9 Applied

#a hierarchical clustering with complete linkage and Euclidean distance, cluster the states
#RANDOMIZATION OF NUMBER
set.seed(2)
hc3.complete=hclust(dist(USArrests), method = "complete")
dev.new()
plot(hc3.complete)

#b Cut the dendrogram at a height that results in three distinct clusters.
cutree(hc3.complete, 3)

#c Hierarchically cluster the states using complete linkage and Eu- clidean distance, 
# after scaling the variables to have standard de- viation one.
sd.data<-scale(USArrests)
hc3.complete.sd=hclust(dist(sd.data, method= "euclidean"), method = "complete")
dev.new()
plot(hc3.complete.sd)

# d  What effect does scaling the variables have on the hierarchical clustering obtained? 
# In your opinion, should the variables be scaled before the inter-observation dissimilarities are computed?
cutree(hc3.complete.sd, 3)

# compare standard deviation cluster and reg cluster
table(cutree(hc3.complete, 3))
table(cutree(hc3.complete.sd, 3))

# the variables should be scaled before the inter-observation dissimilarities are computed are computed because the data measures have different units



#```````````````````````PCA Workshop`````````````

#Chap 10 Conceptual Exercises

I<-matrix(c(1,-2,0,-2,5,0,0,0,2), nrow = 3, ncol=3, byrow = TRUE)


#. Compute the principal components Z1, Z2 and Z3 of the covariance matrix (hint use the eigen() function in R)
eigen(I)
eigen(I)$values
eigen(I)$vectors

#1b. What is the variance of each X variable and what do these variances sum to?
diag(I)
sum(diag(I))
sumvariance<-sum(eigen(I)$values)


#1c. Calculate the proportion of the total variance explained by each principal component. What do these variances sum to?
eigen(I)$values/sumvariances

  
#1d. Convert the covariance matrix above to a correlation matrix1e. 
I.correlation<-cov2cor(I)

#1e. Compute the principal components of the correlation matrix and compare with the solution for the covariance matrix.
eigen(I.correlation)


#1f. Compute the proportion of variance explained by each principal component computed from the correlation matrix
sum(eigen(I.correlation)$values)
prop.variance2<-eigen(I.correlation)$values/3
prop.variance2


#Chapter 10 Lab

#10.4 Lab 1: PCA

states=row.names(USArrests)
states

names(USArrests)

#Note that the apply() function allows us to apply a function—in this case, the mean() function—to each row or column of the data set. 
# The second input here denotes whether we wish to compute the mean of the rows, 1, or the columns, 2
apply(USArrests , 2, mean)

# also examine the variances of the four variables using the apply() function.
apply(USArrests , 2, var)
# If we failed to scale the variables before performing PCA, then most of the principal components that we observed would be driven 
# by the Assault variable

# principal components analysis using the prcomp() func- tion, which is one of several functions in R that perform PCA
# By default, the prcomp() function centers the variables to have mean zero. By using the option scale=TRUE, 
# we scale the variables to have standard deviation one.
pr.out=prcomp(USArrests, scale=TRUE)

names(pr.out)

# The center and scale components correspond to the means and standard deviations of the variables
pr.out$center
pr.out$scale

# The rotation matrix provides the principal component loadings; each col- umn of pr.out$rotation contains the corresponding 
# principal component loading vector
# there are in general min(n − 1, p) informative principal components in a data set with n observations and p variables

pr.out$rotation

# Rather the 50 × 4 matrix x has as its columns the principal component score vectors. That is, the kth column is the kth 
# principal component score vector
dim(pr.out$x)

# plot the first two principal components
# The scale=0 argument to biplot() ensures that the arrows are scaled to represent the loadings; other values for scale give 
# slightly different biplots with different interpretations
dev.new()
biplot(pr.out, scale=0)

# the principal components are only unique up to a sign change
pr.out$rotation=-pr.out$rotation
pr.out$x=-pr.out$x
dev.new()
biplot(pr.out, scale=0)

# The prcomp() function also outputs the standard deviation of each prin- cipal component
pr.out$sdev

# The variance explained by each principal component is obtained by squar- ing
pr.var=pr.out$sdev ^2
pr.var

# the proportion of variance explained by each principal compo- nent, we simply divide the variance explained by each principal component 
# by the total variance
pve=pr.var/sum(pr.var)
pve

# plot the PVE explained by each component, as well as the cumulative PVE
# that the function cumsum() computes the cumulative sum of the elements of a numeric vector
dev.new()
plot(pve, xlab="Principal Component", ylab="Proportion of Variance Explained ", ylim=c(0,1),type='b')
dev.new()
plot(cumsum(pve), xlab="Principal Component ", ylab=" Cumulative Proportion of Variance Explained ", ylim=c(0,1), type='b')



#10.6 Lab 3: NCI60 Data Example

#the NCI60 cancer cell line microarray data, which consists of 6,830 gene expression measurements on 64 cancer cell lines.
library(ISLR)
nci.labs=NCI60$labs
nci.data=NCI60$data

# data has 64 rows and 6,830 columns
dim(nci.data)

# the cancer types for the cell lines
nci.labs[1:4]

# the cancer types for the cell lines
table(nci.labs)


# PCA on the NCI60 Data
# perform PCA on the data after scaling the variables (genes) to have standard deviation one
pr.out=prcomp(nci.data, scale=TRUE)

# first create a simple function that assigns a distinct color to each element of a numeric vector. The function will be used to assign a 
# color to each of the 64 cell lines, based on the cancer type
# the rainbow() function takes as its argument a positive integer, and returns a vector containing that number of distinct colors
Cols=function(vec){
  cols=rainbow(length(unique(vec)))
  return(cols[as.numeric(as.factor(vec))])
}

# plot the principal component score vectors
dev.new()
par(mfrow=c(1,2))
plot(pr.out$x[,1:2], col=Cols(nci.labs), pch=19,xlab="Z1",ylab="Z2")
plot(pr.out$x[,c(1,3)], col=Cols(nci.labs), pch=19,xlab="Z1",ylab="Z3")

# summary of the proportion of variance explained (PVE) of the first few principal components using the summary() method for a prcomp object
summary(pr.out)

# plot the variance explained by the first few principal components
dev.new()
plot(pr.out)

# that the height of each bar in the bar plot is given by squaring the corresponding element of pr.out$sdev
# plot the PVE of each principal component (i.e. a scree plot) and the cu- mulative PVE of each principal component.
pve=100*pr.out$sdev^2/sum(pr.out$sdev^2)
dev.new()
par(mfrow=c(1,2))
plot(pve,  type="o", ylab="PVE", xlab="Principal Component", col="blue")
plot(cumsum(pve), type="o", ylab="Cumulative PVE", xlab="Principal Component", col="brown3")




# Clustering the Observations of the NCI60 Data

# To begin, we standardize the variables to have mean zero and standard deviation one.
sd.data=scale(nci.data)

# perform hierarchical clustering of the observations using complete, single, and average linkage. Euclidean distance is used as the dissimilarity measure.
dev.new()
par(mfrow=c(1,3))
data.dist=dist(sd.data)
plot(hclust(data.dist, method = "complete"), labels=nci.labs, main="Complete Linkage", xlab="", sub="",ylab="")
plot(hclust(data.dist, method="average"), labels=nci.labs, main="Average Linkage", xlab="", sub="",ylab="")
plot(hclust(data.dist, method="single"), labels=nci.labs,  main="Single Linkage", xlab="", sub="",ylab="")

# cut the dendrogram at the height that will yield a particular number of clusters, say four
hc.out=hclust(dist(sd.data))
hc.clusters=cutree(hc.out,4)
table(hc.clusters,nci.labs)

# plot the cut on the dendrogram that produces these four clusters
# The abline() function draws a straight line on top of any existing plot in R. The argument h=139 plots a horizontal line at 
# height 139 on the den- drogram; this is the height that results in four distinct clusters. 
dev.new()
par(mfrow=c(1,1))
plot(hc.out, labels=nci.labs)
abline(h=139, col="red")

# Printing the output of hclust gives a useful brief summary of the object
hc.out

# clustering results compare to what we get if we perform K-means clustering with K = 4
set.seed(2)
km.out=kmeans(sd.data, 4, nstart=20)
km.clusters=km.out$cluster
table(km.clusters,hc.clusters)

# Rather than performing hierarchical clustering on the entire data matrix, we can simply perform hierarchical clustering on the 
# first few principal component score vectors
hc.out=hclust(dist(pr.out$x[,1:5]))
dev.new()
plot(hc.out, labels=nci.labs, main="Hier. Clust. on First Five Score Vectors")
table(cutree(hc.out,4), nci.labs)



#

dim(Sparrow2.csv)
#if off diagonal are really small number then PCA will not be a good method
round(cor(Sparrow2.csv),2)


#1. The loadings (which variables are loaded more heavily on the differentpcs
# PC1 has the heaviest loading
pr.out=prcomp(Sparrow2.csv, scale=TRUE)
pr.out$rotation
View(pr.out$rotation)

#2. What does the scree plot look like? Where is the elbow? How many PC’s adequately capture the total variation in the sparrows data. 
# What variance is explained by each PC?
dev.new()
par(mfrow=c(1,2))
pve=100*pr.out$sdev^2/sum(pr.out$sdev^2)
plot(pve,  type="o", ylab="PVE", xlab="Principal Component", col="blue")
plot(cumsum(pve), type="o", ylab="Cumulative PVE", xlab="Principal Component", col="brown3")

#3. Plot the principal components eg PC1 vs PC2, PC1 vs PC3 etc – arethere any interesting features? 
# Clusters perhaps? Outliers?
dev.new()
par(mfrow=c(1,2))
plot(pr.out$x[,1:2], col=1, pch=19,xlab="PC1",ylab="PC2")
plot(pr.out$x[,c(1,3)], col=1, pch=19,xlab="PC1",ylab="PC3")

# 4. Plot the biplot – which loading vectors align close to each other and hence which variables are more correlated with each other.Conversely which variables are less correlated with each other. Are any loading vectors aligned with any cluster of sparrows?
dev.new()
par(mfrow=c(1,1))
biplot(pr.out, scale=0, cex=0.5)










