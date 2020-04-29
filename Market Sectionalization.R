###########################################################

## HarvardX(125.9X) - Capstone
## Name: NIRMAL SAI SWAROOP JANAPANEEDI
## Choose Your Own Project

###### Market Sectionalization ######

### Importing Dataset ###
customer_data = read.csv("C:/Users/Dell/Desktop/Nirmal/Mall_Customers.csv")
customer_data

# NOTE: Makesure the evaluater has the above .csv file loaded by chaging the path accordingly.

### Essential Packages ###
if(!require(plotrix)) install.packages("plotrix", repos = "http://cran.us.r-project.org")
if(!require(ggplot2)) install.packages("ggplot2", repos = "http://cran.us.r-project.org")
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(purrr)) install.packages("purrr", repos = "http://cran.us.r-project.org")
if(!require(cluster)) install.packages("cluster", repos = "http://cran.us.r-project.org")
if(!require(grid)) install.packages("grid", repos = "http://cran.us.r-project.org")
if(!require(gridExtra)) install.packages("gridExtra", repos = "http://cran.us.r-project.org")
if(!require(NbClust)) install.packages("Nbclust", repos = "http://cran.us.r-project.org")
if(!require(factoextra)) install.packages("factoextra", repos = "http://cran.us.r-project.org")
if(!require(dplyr)) install.packages("dplyr", repos = "http://cran.us.r-project.org")

### Loading the required library ###
library(plotrix)
library(ggplot2)
library(tidyverse)
library(purrr)
library(cluster)
library(grid)
library(gridExtra)
library(NbClust)
library(factoextra)
library(dplyr)

# NOTE: Across this project of Market Sectionalization for the ease of understanding the term "Customer" will be used.

### Data Insight/Preparation ###
str(customer_data)
summary(customer_data)
names(customer_data)

## Data Visualization ###

# 1.Gender 
a = table(customer_data$Gender)
barplot(a,main = "Using BarPlot to display Gender Comparision",
        ylab = "Count",
        xlab = "Gender",
        col = rainbow(2),
        legend = rownames(a)) # Bar plot visualization

pct = round(a/sum(a) * 100)
lbs = paste(c("Female","Male")," ",pct,"%",sep=" ")
pie3D(a,labels = lbs,
      main="Pie Chart Depicting Ratio of Female and Male") # Pie chart visualization

# 2.Age
hist(customer_data$Age,
     col = "blue",
     main = "Histogram to Show Count of Age Class",
     xlab = "Age Class",
     ylab = "Frequency",
     labels = TRUE) # Visualization through Histogram

boxplot(customer_data$Age,
        col = "blue",
        main = "Boxplot for Descriptive Analysis of Age") # Boxplot Visualization

# 3.Annual Income of the Customers
hist(customer_data$Annual.Income..k..,
     col = "#660033",
     main = "Histogram for Annual Income",
     xlab = "Annual Income Class",
     ylab = "Frequency",
     labels = TRUE) # Visualization through Histogram

plot(density(customer_data$Annual.Income..k..),
     col = "yellow",
     main = "Density Plot for Annual Income",
     xlab = "Annual Income Class",
     ylab = "Density")
polygon(density(customer_data$Annual.Income..k..),
        col = "#ccff66") # Visualization through Density plot

# 4.Spending score of the Customers
boxplot(customer_data$Spending.Score..1.100.,
        horizontal = TRUE,
        col = "red",
        main = "BoxPlot for Descriptive Analysis of Spending Score") # Boxplot Visualization

hist(customer_data$Spending.Score..1.100.,
     main = "HistoGram for Spending Score",
     xlab = "Spending Score Class",
     ylab = "Frequency",
     col = "#6600cc",
     labels = TRUE) # Visualization through Histogram

### K-means ML algorithm ###

# k-means clustering/algorithm is a method of vector quantization, originally from signal processing, that aims to partition n observations into k clusters in which each observation belongs to the cluster with the nearest mean, serving as a prototype of the cluster. 
# We will use this algorithm to make clusters with the help of which we analyse how market sectionalization works.

# 1.Elbow Method

set.seed(123)
# function to calculate total intra-cluster sum of square 
iss <- function(k) {
  kmeans(customer_data[,3:5],k,iter.max=100,nstart=100,algorithm="Lloyd" )$tot.withinss
}
k.values <- 1:10
iss_values <- map_dbl(k.values, iss)
plot(k.values, iss_values,
     type = "b", pch = 19, frame = FALSE, 
     xlab = "Number of clusters K",
     ylab = "Total intra-clusters sum of squares")

# 2.Average Silhouette Method

k2<-kmeans(customer_data[,3:5],2,iter.max = 100,nstart = 50,algorithm  ="Lloyd")
s2<-plot(silhouette(k2$cluster,dist(customer_data[,3:5],"euclidean")))

k3<-kmeans(customer_data[,3:5],3,iter.max = 100,nstart = 50,algorithm = "Lloyd")
s3<-plot(silhouette(k3$cluster,dist(customer_data[,3:5],"euclidean")))

k4<-kmeans(customer_data[,3:5],4,iter.max = 100,nstart = 50,algorithm = "Lloyd")
s4<-plot(silhouette(k4$cluster,dist(customer_data[,3:5],"euclidean")))

k5<-kmeans(customer_data[,3:5],5,iter.max = 100,nstart = 50,algorithm = "Lloyd")
s5<-plot(silhouette(k5$cluster,dist(customer_data[,3:5],"euclidean")))

k6<-kmeans(customer_data[,3:5],6,iter.max = 100,nstart = 50,algorithm = "Lloyd")
s6<-plot(silhouette(k6$cluster,dist(customer_data[,3:5],"euclidean")))

k7<-kmeans(customer_data[,3:5],7,iter.max = 100,nstart = 50,algorithm = "Lloyd")
s7<-plot(silhouette(k7$cluster,dist(customer_data[,3:5],"euclidean")))

k8<-kmeans(customer_data[,3:5],8,iter.max = 100,nstart = 50,algorithm = "Lloyd")
s8<-plot(silhouette(k8$cluster,dist(customer_data[,3:5],"euclidean")))

k9<-kmeans(customer_data[,3:5],9,iter.max = 100,nstart = 50,algorithm = "Lloyd")
s9<-plot(silhouette(k9$cluster,dist(customer_data[,3:5],"euclidean")))

k10<-kmeans(customer_data[,3:5],10,iter.max = 100,nstart = 50,algorithm = "Lloyd")
s10<-plot(silhouette(k10$cluster,dist(customer_data[,3:5],"euclidean")))

# Building clusters 
fviz_nbclust(customer_data[,3:5], kmeans, method = "silhouette")

# 3.Gap Statistic Method

set.seed(125)
stat_gap <- clusGap(customer_data[,3:5], FUN = kmeans, nstart = 25,
                    K.max = 10, B = 50)
fviz_gap_stat(stat_gap)

# We will be taking k = 6 as our optimal cluster.

k6<-kmeans(customer_data[,3:5],6,iter.max = 100,nstart = 50,algorithm = "Lloyd")
k6

### Final Prediction using Clusters ###

# Visualizing the Clustering Results using the First Two Principle Components
pcclust = prcomp(customer_data[,3:5],scale=FALSE) #principal component analysis
summary(pcclust)
pcclust$rotation[,1:2]

# Visualise the cluster
set.seed(1)
ggplot(customer_data, aes(x = Annual.Income..k.., y = Spending.Score..1.100.)) + 
  geom_point(stat = "identity", aes(color = as.factor(k6$cluster))) +
  scale_color_discrete(name = " ",
                       breaks = c("1", "2", "3", "4", "5","6"),
                       labels = c("Cluster 1", "Cluster 2", "Cluster 3", "Cluster 4", "Cluster 5","Cluster 6")) +
  ggtitle("Segments of Mall Customers", subtitle = "Using K-means Clustering")


ggplot(customer_data, aes(x = Spending.Score..1.100., y =Age)) + 
  geom_point(stat = "identity", aes(color = as.factor(k6$cluster))) +
  scale_color_discrete(name = " ",
                       breaks = c("1", "2", "3", "4", "5","6"),
                       labels = c("Cluster 1", "Cluster 2", "Cluster 3", "Cluster 4", "Cluster 5","Cluster 6")) +
  ggtitle("Segments of Mall Customers", subtitle = "Using K-means Clustering")


kCols = function(vec){cols = rainbow (length (unique (vec)))
return (cols[as.numeric(as.factor(vec))])}
digCluster<-k6$cluster; dignm<-as.character(digCluster); # K-means clusters
plot(pcclust$x[,1:2], col = kCols(digCluster),pch = 19,xlab = "K-means",ylab = "classes")
legend("bottomleft",unique(dignm),fill = unique(kCols(digCluster)))

# So, these give the predictions using the cluster visualizations.

### Conclusion ###

# In this data science project, we went through the market sectionalization, also known as customer segmentation model.
# We developed this using a class of machine learning known as unsupervised learning.
# Specifically, we made use of a clustering algorithm called K-means clustering.
# We analyzed and visualized the data and then proceeded to implement our algorithm.

## Hope you have enjoyed exploring and analyzing my capstone project(Market Sectionalization), using Machine Learning approach in R.

################## Thank You. ##################

###########################################################
###########################################################