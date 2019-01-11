# Install and load packages
install.packages("NbClust")
library(NbClust)

# Load data with the name cldata

# Review data
summary(cldata)

# Scale data
testdata <- cldata
testdata <- scale(testdata)

# Determine number of clusters. Option 1: visual rule
wss[1] <- (nrow(testdata)-1)*sum(apply(testdata,2,var))
for (i in 1:15) wss[i] <- sum(kmeans(testdata,
                                     centers=i)$withinss)
plot(1:15, wss, type="b", xlab="Number of Clusters", ylab="Within groups sum of squares")

# Determine number of clusters. Option 2: more frequent optimal number
res <- NbClust(cldata, diss=NULL, distance = "euclidean",
     min.nc=2, max.nc=12,
     method = "kmeans", index = "all")
res$Best.partition

# K-Means Cluster Analysis (based on the proposed number by NbCluster)
options(digits = 2)
fit <- kmeans(testdata, 3)
table(fit$cluster)
# Calculate average for each cluster
aggregate(cldata,by=list(fit$cluster),FUN=mean)
# Add segmentation to dataset
cldata.w.cluster <- data.frame(cldata, fit$cluster)




#EXTRA STEP
library(cluster)
clusplot(cldata, fit$cluster, color=TRUE, shade=TRUE, 
         labels=4, lines=0, main="K-means cluster plot")

#Hierarchical Clustering
library(cluster)
cldata.dist <- dist(cldata) # ??
cldata.hc <- hclust(cldata.dist, method="complete") #hieraricical clustirng

plot(cldata.hc)
rect.hclust(cldata.hc, k=4, border="red")

cldata.hc.segment <- cutree(cldata.hc, k=4)     # membership vector for 4 groups
table(cldata.hc.segment)

