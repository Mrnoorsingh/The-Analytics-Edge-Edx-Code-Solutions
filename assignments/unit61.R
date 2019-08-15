article=read.csv("dailykos.csv")
str(article)
distance=dist(article,method = "euclidean")
cluster=hclust(distance,method="ward.D")
plot(cluster)#choices for number of clusters are decided from dendrogram
clustergroup=cutree(cluster,k=7)
spl=split(article,clustergroup)
str(spl[[3]])
cls_no=subset(article,clustergroup==1)
tail(sort(colMeans(subset(article,clustergroup==7))))

k=7
set.seed(1000)
cluster_k=kmeans(article,centers=k)
clsk_no=subset(article,cluster_k$cluster==2)
table(clsk_no,cls_no)
table(cls_no)
tail(sort(colMeans(subset(article,cluster_k$cluster==2))))


#most matched cluster 
table(clustergroup,cluster_k$cluster)
