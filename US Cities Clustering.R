library(NbClust)

# Scale data
# testdata <- US_Cities
US_Cities <- subset(US_Cities,City!="Honolulu")
 
testdata <- scale(US_Cities[2:6])

# Determine number of clusters. Option 1: visual rule
set.seed(42)
wss <- (nrow(testdata)-1)*sum(apply(testdata,2,var))
for (i in 2:15) wss[i] <- sum(kmeans(testdata,
                                     centers=i)$withinss)
plot(1:15, wss, type="b", xlab="Number of Clusters", ylab="Within groups sum of squares")

# Determine number of clusters. Option 2: more frequent optimal number
res <- NbClust(testdata, diss=NULL, distance = "euclidean",
               min.nc=2, max.nc=12,
               method = "kmeans", index = "all")
res$Best.partition

# K-Means Cluster Analysis (based on the proposed number by NbCluster)
options(digits = 2)
fit <- kmeans(testdata, 4)
table(fit$cluster)
# Calculate average for each cluster
aggregate(US_Cities[2:6],by=list(fit$cluster),FUN=mean)
# Add segmentation to dataset
US.w.cluster <- data.frame(US_Cities, fit$cluster)

library(cluster)
#EXTRA STEP
clusplot(testdata, fit$cluster, color=TRUE, shade=TRUE, 
         labels=4, lines=0, main="K-means cluster plot")

#Hierarchical Clustering

cldata.dist <- dist(testdata)
cldata.hc <- hclust(cldata.dist, method="complete")

plot(cldata.hc)
rect.hclust(cldata.hc, k=3, border="red")

cldata.hc.segment <- cutree(cldata.hc, k=3)     # membership vector for 4 groups
table(cldata.hc.segment)

