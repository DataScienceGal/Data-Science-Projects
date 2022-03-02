###########################################
### PRINCIPLE COMPONENTS ANALYSIS (PCA) ###
###########################################
#
# GOALS:
# 1 - find linear combos of variables to create PCs
#     * combines low variance and correlated variables
# 2 - Maintain most variance in the data
#     * PC1 has highest variance, PC2 has second highest, ...
# 3 - PCs are orthogonal (uncorrelated )
#     * Which prevents collinearity
# PCs = set of high-variance, perpendicular/orthogonal predictors (independent)
###########################################
#
# Steps:
# 1 - Center/Scale data
# 2 - Missing data drop/impute
# 3 - features that are categorical
#     a. do not include
#     b. encode as numbers
# 4 - PC analysis find regression line that explains two-dimensions
#
###########################################
# PRCOMP FUNCTION:
# center: the column means used to center to the data, or FALSE if the data weren't centered
# scale: the column standard deviations used to scale the data, or FALSE if the data weren't scaled
# rotation: the directions of the principal component vectors in terms of the original features/variables. This information allows you to define new data in terms of the original principal components
# x: the value of each observation in the original dataset projected to the principal components
###########################################
# The loadings, represented as vectors, explain the mapping from the 
# original features to the principal components.
set.seed(1234)


####### IRIS DATA
data(iris)
head(iris)
summary(iris)
dim(iris)
names(iris)

####### PCA in R
pr.iris=prcomp(x=iris[-5],scale=FALSE,center=TRUE)
summary(pr.iris) # variance performed by each PC
# the first PC describes over 90% of the variablity in the data

#### BIPLOT
# Plots both the PC loadings and the mapping of the obs.
# to their first two PCs.
# FIRST TWO PCs with Features mapped onto them
# If two features face the same direction -> correlated
pr.iris = prcomp(x=iris[-5],scale=FALSE, center=TRUE)
biplot(pr.iris)


##### SCREE PLOT
# Proportion of variance explained by each PC
# Cumulative % of variance as number of PCs increases

pr.var = pr.iris$sdev^2 # square the SDs to get Var
pve=pr.var/sum(pr.var) # percentage
plot(pve,xlab='PCs',ylab='Proportion of Variance Explained',
     ylim=c(0,1),type="b")





###### POKEMON DATA
pr.out=prcomp(pokemon,scale=TRUE)
summary(pr.out)
# Take 2 PCs to maintain at least 75% of the variance
pr.out$rotation
pr.out$center

dim(pr.out$rotation)  #4 features x 4 PCs
dim(pokemon) #50 obs x 4 features

biplot(pr.out,scale=TRUE,center=TRUE)

# Variability of each principal component: pr.var
pr.var <- pr.out$sdev^2

# Variance explained by each principal component: pve
pve <- pr.var / sum(pr.var)


##### scree plots
# Plot variance explained for each principal component
plot(pve, xlab = "Principal Component",
     ylab = "Proportion of Variance Explained",
     ylim = c(0, 1), type = "b")

# Plot cumulative proportion of variance explained
plot(cumsum(pve), xlab = "Principal Component",
     ylab = "Cumulative Proportion of Variance Explained",
     ylim = c(0, 1), type = "b")


##### COMPARISON WITH AND WITHOUT SCALING
### Data should be scaled. Total column has a lot of variation
# After scaling the data, there's a much more even distribution of the loading vectors

# Mean of each variable
colMeans(pokemon)

# Standard deviation of each variable
apply(pokemon, 2, sd)

# PCA model with scaling: pr.with.scaling
pr.with.scaling=prcomp(pokemon,scale=TRUE)

# PCA model without scaling: pr.without.scaling
pr.without.scaling=prcomp(pokemon,scale=FALSE)

# Create biplots of both for comparison
biplot(pr.with.scaling)
biplot(pr.without.scaling)
