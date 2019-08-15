#reading txt file
movies=read.table("movie_data.txt",header = FALSE,sep = "|",quote = "\"")#header is False as no variable row in the file



colnames(movies) = c("ID", "Title", "ReleaseDate", "VideoReleaseDate", "IMDB", "Unknown", "Action", "Adventure", "Animation", "Childrens", "Comedy", "Crime", "Documentary", "Drama", "Fantasy", "FilmNoir", "Horror", "Musical", "Mystery", "Romance", "SciFi", "Thriller", "War", "Western")

# Remove unnecessary variables
movies$ID = NULL
movies$ReleaseDate = NULL
movies$VideoReleaseDate = NULL
movies$IMDB = NULL
#removing duplicate rows
movies = unique(movies)

table(movies$Drama & movies$Romance)

# Compute distances
distances = dist(movies[2:20], method = "euclidean")#column 1 is movie titles

# Hierarchical clustering
clusterMovies = hclust(distances, method = "ward.D")
# Plot the dendrogram
plot(clusterMovies)

# Assign points to clusters
clusterGroups = cutree(clusterMovies, k = 2)#k means k clusters


#Now let's figure out what the clusters are like.
# Let's use the tapply function to compute the percentage of movies in each genre and cluster


tapply(movies$Action,clusterGroups, mean)#mean gives proportion when variable values are 0 and 1


# Find which cluster Men in Black is in.

subset(movies, Title=="Men in Black (1997)")#gives 257 index
clusterGroups[257]#gives cluster number

# Create a new data set with just the movies from cluster 2
cluster2 = subset(movies, clusterGroups==2)

# Look at the first 10 titles in this cluster:
cluster2$Title[1:10]


#In this video, we explain how you can find the cluster centroids by using the function
#"tapply" for each variable in the dataset. While this approach works and is familiar to us,
#it can be a little tedious when there are a lot of variables. An alternative approach is to 
#use the colMeans function. With this approach, you only have one command for each cluster
#instead of one command for each variable. If you run the following command in your R console, 
#you can get all of the column (variable) means for cluster 1:

#column means (proportion ) for cluster 1
colMeans(subset(movies[2:20], clusterGroups == 1))

#split data based on their cluster
spl = split(movies[2:20], clusterGroups)
spl[[1]]#is same as subset(movies[2:20], clusterGroups == 1)
colMeans(spl[[1]])#same as colMeans(subset(movies[2:20], clusterGroups == 1))

#output the cluster centroids for all clusters
lapply(spl, colMeans)
