getwd()
setwd("C:/Users/sruth/OneDrive/Desktop/CSE4027 LAB")
dir()
da=read.csv("customer_segmentation_cleaned.csv")
da

#Customer Gender Visualization
a=table(da$Gender)
barplot(a,main="Using BarPlot to display Gender Comparision",
        ylab="Count",
        xlab="Gender",
        col=rainbow(2),
        legend=rownames(a))

#visualizing a pie chart to observe the ratio of male and female distribution.
pie=round(a/sum(a)*100)
lbs=paste(c("Female","Male")," ",pie,"%",sep=" ")
library(plotrix)
pie3D(a,labels=lbs,
      main="Pie Chart Depicting Ratio of Female and Male")

#Analysing the Age
#Let us plot a histogram to view the distribution 
#to plot the frequency of customer ages.
#We will first proceed by taking summary of the Age variable.

hist(da$Age,
     col="red",
     main="Histogram to Show Count of Age Class",
     xlab="Age Class",
     ylab="Frequency",
     labels=TRUE)
#Boxplot for Descriptive Analysis of Age
boxplot(da$Age,
        col="blue",
        main="Boxplot for Descriptive Analysis of Age")

#Analysing the spending score of the customers
b=table(da$Spending_Score)
barplot(b,main="Using BarPlot to display spending score Comparision",
        ylab="Count",
        xlab="spending score",
        col=rainbow(2),
        legend=rownames(b))

#  To specify the number of clusters 
# using Elbow Method
library(purrr)
set.seed(123)

el <- function(k) {
  kmeans(da[4],k,iter.max=100,nstart=100,algorithm="Lloyd" )$tot.withinss
}

k.values <- 1:15


ell_values <- map_dbl(k.values, el)

plot(k.values, ell_values,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total intra-clusters sum of squares")



# From the above graph, we conclude that 2 
#is the appropriate number of clusters 
#since it seems to be appearing at the bend in the elbow plot.

# 2nd Method
#Average Silhouette Method
library(cluster) 
library(gridExtra)
library(grid)

avg_sil <- function(k) {
  km.res <- kmeans(da[4], centers = k, nstart = 25)
  ss <- silhouette(km.res$cluster, dist(da))
  mean(ss[3])
}

# Compute and plot wss for k = 2 to k = 15
k.values <- 2:15

# extract avg silhouette for 2-15 clusters
avg_sil_values <- map_dbl(k.values, avg_sil)

plot(k.values, avg_sil_values,
     type = "b", pch = 19, frame = FALSE, 
     xlab = "Number of clusters K",
     ylab = "Average Silhouettes")

library(NbClust)
library(factoextra)

fviz_nbclust(da[4], kmeans, method = "silhouette")

#3rd Method
#Gap Static Method

set.seed(125)
stat_gap <- clusGap(da[4], FUN = kmeans, nstart = 25,
                    K.max = 10, B = 50)
fviz_gap_stat(stat_gap)

#Now, let us take k = 2 as our optimal cluster 

k2<-kmeans(da[4],2,iter.max=100,nstart=50,algorithm="Lloyd")
k2

pcclust=prcomp(da[4],scale=FALSE) #principal component analysis
summary(pcclust)

#Visualizing The Clusters
set.seed(1)
ggplot(da, aes(x =da$Age , y = da$Work_Experience)) + 
  geom_point(stat = "identity", aes(color = as.factor(k2$cluster))) +
  scale_color_discrete(name=" ",
                       breaks=c("1", "2"),
                       labels=c("Cluster 1", "Cluster 2")) +
  ggtitle("Segments of Customers", subtitle = "Using K-means Clustering")

