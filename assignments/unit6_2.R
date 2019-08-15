airline=read.csv("AirlinesCluster.csv")


#preprocessing and normalize data
library(caret)
pre=preProcess(airline)#preprocess
air_norm=predict(pre,airline)#normalize

#hierarchical clustering
distance=dist(air_norm,method="euclidean")
cluster_h=hclust(distance,method = "ward.D")
plot(cluster_h)


#dividing into 5 clusters
clustergroup=cutree(cluster_h,k=5)
spl=split(air_norm,clustergroup)
str(spl[[1]])

lapply(spl, colMeans)


#k means clustering

k=5
set.seed(88)
cluster_k=kmeans(air_norm,centers = k,iter.max = 1000)
spl1=split(air_norm,cluster_k$cluster)
nrow(spl1[[5]])# clusters number of observations
lapply(spl1,colMeans )


#do you expect cluster 1 to be similiar to hier cluster 1?
#The clusters are not displayed in a meaningful order, so while there may be a 
#cluster produced by the k-means algorithm that is similar to Cluster 1 produced by 
#the Hierarchical method, it will not necessarily be shown first.