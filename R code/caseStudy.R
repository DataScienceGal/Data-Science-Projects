############### UNSUPERVISED LEARNING: CASE STUDY ####################
# The dataset was published in a paper by Bennett and Mangasarian. 
# Data consists of measurements of nuclei of cells of human breast masses. 
# Each observation is of a single mass or group of cells and consists of ten features. 
# Each feature is a summary statistic of measurements from the cells in that mass. 
# We are not using the target label so the analysis can be unsupervised. 
#
# Source: K. P. Bennett and O. L. Mangasarian: 
# "Robust Linear Programming Discrimination of Two Linearly Inseparable Sets"
#####################################################################
set.seed(1234)

url <- "http://s3.amazonaws.com/assets.datacamp.com/production/course_1903/datasets/WisconsinCancer.csv"

# Download the data: wisc.df
wisc.df=read.csv(url)
wisc.data=as.matrix(wisc.df[,3:32])
row.names(wisc.data) <- wisc.df$id

# Create diagnosis vector: 1=Malignant (M); 0=Benign
diagnosis <- as.numeric(wisc.df$diagnosis == "M")

##### EDA 

# do we need to center and scale data?
# Check column means and standard deviations
round(colMeans(wisc.data),4)
round(apply(wisc.data,2,sd),4)

# some means are quite different from zero
# some variables are quite different variablity


# Execute PCA
wisc.pr=prcomp(wisc.data,scale=TRUE,center=TRUE)
summary(wisc.pr)


####### Biplot 
biplot(wisc.pr)


####### Scatterplots of PCs
# Scatter plot observations by components 1 and 2
plot(wisc.pr$x[, c(1, 2)], col = (diagnosis + 1), 
     xlab = "PC1", ylab = "PC2")

# Repeat for components 1 and 3
plot(wisc.pr$x[,c(1,3)], col = (diagnosis + 1), 
     xlab = "PC1", ylab = "PC3")

# Repeat for components 1 and 4
plot(wisc.pr$x[,c(1,4)], col = (diagnosis + 1), 
     xlab = "PC1", ylab = "PC4")



####### Scree Plots
# Set up 1 x 2 plotting grid
par(mfrow = c(1, 2))

# Calculate variability of each component
pr.var=wisc.pr$sdev**2

# Variance explained by each principal component: pve
pve=pr.var/sum(pr.var)

# Plot variance explained for each principal component
plot(pve, xlab = "Principal Component", 
     ylab = "Proportion of Variance Explained", 
     ylim = c(0, 1), type = "b")

# Plot cumulative proportion of variance explained
plot(cumsum(pve), xlab = "Principal Component", 
     ylab = "Cumulative Proportion of Variance Explained", 
     ylim = c(0, 1), type = "b")


### We want enough PCs to explain 80% of variance
# Maybe 4 or 5 PCs.


### Next: perform clustering (2 types) and combine with PCA
# For the first PC, find the component of the loading vector 
# for the feature concave.points_mean
wisc.pr$rotation["concave.points_mean",1]


# Scale data
data.scaled = scale(wisc.data)

# Calculate the (Euclidean) distances
data.dist=dist(data.scaled)

# Create a hierarchical clustering model
wisc.hclust=hclust(data.dist,method="complete")
plot(wisc.hclust) # At height of 20, there are 4 clusters
wisc.hclust.clusters=cutree(wisc.hclust,k=4) # Cut tree with 4 clusters



# k-means
wisc.km=kmeans(scale(wisc.data),centers=2,nstart=20)


# Compare methods to actual diagnoses
table(diagnosis,wisc.km$cluster)
table(diagnosis,wisc.hclust.clusters)
table(wisc.km$cluster,wisc.hclust.clusters)

# like clusters 1, 2, and 4 from the hierarchical clustering model 
# can be interpreted as the cluster 1 equivalent from k-means algo,
# and cluster 3 can be interpreted as the cluster 2 equivalent


# hierarchical cluster on PCs
wisc.pr.hclust <- hclust(dist(wisc.pr$x[,1:7]), method = "complete")
wisc.pr.hclust.clusters=cutree(wisc.pr.hclust,k=4) # 4 clusters

# Compare to actual diagnoses
table(diagnosis, wisc.pr.hclust.clusters)
table(wisc.km$cluster,wisc.pr.hclust.clusters)
table(diagnosis, wisc.hclust.clusters)
table(diagnosis, wisc.km$cluster)
