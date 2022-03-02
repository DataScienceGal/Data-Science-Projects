#############  K-MEANS CLUSTERING ###########

# The model runs iteratively until not of the 
# points change clusters. Default is 10.
# Algorithm may not always converge.

# Tuning parameter: # clusters

# Best outcome: the outcome that minimizes the 
# total within cluster sum of square errors.

# Kmeans randomly initializes the centers of clusters
# Consequently, observations can be assigned to different clusters
# Also, the result could find local minima for the kmeans algo

##############################################
set.seed(1)

## Synethetic data generated from 3 subgroups
## select best number of subgroups
# X:  some two-dimensional data
# run 20 times- algo with 3 centers
km.out =kmeans(x,centers=3,nstart=20)
summary(km.out)

# Print the cluster membership component of the model (values of 1,2,3)
km.out$cluster


######## RUN KMEANS 6 TIMES WHERE IT INITIALIZES EACH TIME ########
# Set up 2 x 3 plotting grid
par(mfrow = c(2, 3))


for(i in 1:6) {
  # Run kmeans() on x with three clusters and one start
  km.out <- kmeans(x, centers=3, nstart=1)
  
  # Plot clusters
  plot(x, col = km.out$cluster, 
       main = km.out$tot.withinss, 
       xlab = "", ylab = "")
}

##### BUILD 15 KMEANS MODELS EACH WITH DIFFERENT # CLUSTERS
# Initialize total within sum of squares error: wss
wss <- 0

# For 1 to 15 cluster centers
for (i in 1:15) {
  km.out <- kmeans(x, centers = i, nstart=20)
  # Save total within sum of squares to wss variable
  wss[i] <- km.out$tot.withinss
}

# Plot total within sum of squares vs. number of clusters
plot(1:15, wss, type = "b", 
     xlab = "Number of Clusters", 
     ylab = "Within groups sum of squares")

# Set k equal to the number of clusters corresponding to the elbow location
k <- 2


####################
### POKEMON DATA ###
####################
# https://www.kaggle.com/abcsds/pokemon
# https://pokemondb.net/pokedex
pokemon=read.csv('C:\\Users\\stef_\\Documents\\R\\Unsupervised Learing in R\\1-Unsupervised Learning in R\\Pokemon.csv')

# STEPS:
# 1-Select variables to Cluster
# 2-Scale data
head(pokemon)


# Initialize total within sum of squares error: wss
wss <- 0

# Look over 1 to 15 possible clusters
for (i in 1:15) {
  # Fit the model: km.out
  km.out <- kmeans(pokemon, centers = i, nstart = 20, iter.max = 50)
  # Save the within cluster sum of squares
  wss[i] <- km.out$tot.withinss
}

# Produce a scree plot
plot(1:15, wss, type = "b", 
     xlab = "Number of Clusters", 
     ylab = "Within groups sum of squares")

# Select number of clusters
k <- 6

# Build model with k clusters: km.out
km.out <- kmeans(pokemon, centers = k, nstart = 20, iter.max = 50)

# View the resulting model
km.out

# Plot of Defense vs. Speed by cluster membership
plot(pokemon[, c("Defense", "Speed")],
     col = km.out$cluster,
     main = paste("k-means clustering of Pokemon with", k, "clusters"),
     xlab = "Defense", ylab = "Speed")