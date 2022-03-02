#######################################################
### HIERARCHICAL CLUSTERING ####
#######################################################
# Use when # Clusters is not known ahead of time
# Only one parameter = distance between the observations
# 1 - Starts with each point being its own cluster
# 2 - find the Closest two clusters and join to form a new cluster
# 3 - continue iteratively until only a single cluster
#######################################################
# If you are using average-linkage, then height would be 0.25(d(9,8)+d(9,10)+d(11,8)+d(11,10)), 
# where d(a,b) is the distance (correlation distance) between instances a and b

# Balanced trees are essential if you want an even number of observations 
# assigned to each cluster. On the other hand, if you want to detect 
# outliers, for example, an unbalanced tree is more desirable because
# pruning an unbalanced tree can result in most observations assigned
# to one cluster and only a few observations assigned to other clusters

# Make sure to scale data before using 
# data on different scales can cause undesirable results
set.seed(1234)

# assume x has two dimensions

colMeans(x)
apply(x, 2, sd) # apply to columns (second item of matrix)
scaled_x=scale(x)

#validate it is normalized
colMeans(scaled_x)
apply(scaled_x,2,sd)

# Euclidean distance
dist_matrix = dist(x)

# hierarchical clustering
hclust.out=hclust(d=dist_matrix)
summary(hclust.out)
plot(hclust.out)
# height is the distance between clusters
# i.e., if two clusters merge at height x, then the two clusters are x distance apart
abline(h=6,col="red") # 6 is height - wants all obs no further apart than this height
cutree(hclust.out, h=6) # cut by height
cutree(hclust.out, k=2) # cut by number of clusters
## The output of cutree represents the cluster assignments for each observation

# Dendrogram
plot(as.dendrogram(hclust.out), main="dendrogram")

# add two rectangle to dendrogram with 3 clusters
# border argument changes colors of rectangles
rect.hclust(hclust.out, k=3, which=c(1,3), border=3:4)
rect.hclust(hclust.out, h=150)

#################### LINKAGE #####################
# Methods for calculating distance between clusters (linkage)
# 1 - complete: largest distance between all observations in cluster 1 and cluster 2
# 2 - single:  smallest distance between all observations in cluster 1 and cluster 2
# 3 - average:   smallest distance between all observations in cluster 1 and cluster 2
# 4 - centroid:  distance between centroid of cluster 1 and centroid of cluster 2
# complete and average produce more balanced trees
# centroid can create inversion - where clusters are fused below the cluster - bad


hclust.complete=hclust(d, method="complete")
hclust.average=hclust(d, method="average")
hclust.single=hclust(d, method="single")


# Plot dendrograms
plot(hclust.complete,main="Complete")
plot(hclust.average,main="Average")
plot(hclust.single,main="Single")


########################
####   POKEMON DATA ####
########################
# View column means
colMeans(pokemon)

# View column standard deviations
apply(pokemon,2,sd)

# Scale the data
pokemon.scaled=scale(pokemon)

# Create hierarchical clustering model: hclust.pokemon
hclust.pokemon=hclust(dist(pokemon.scaled),method="complete")

# The results from running k-means clustering on the pokemon data 
# (for 3 clusters) are stored as km.pokemon. 

# Apply cutree() to hclust.pokemon: cut.pokemon
cut.pokemon=cutree(hclust.pokemon,k=3)

# Compare methods
table(km.pokemon$cluster, cut.pokemon)

# Looking at the table, it looks like the hierarchical 
# clustering model assigns most of the observations to cluster 1,
# while the k-means algorithm distributes the observations 
# relatively evenly among all clusters