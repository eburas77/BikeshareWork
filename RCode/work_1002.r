# K Means clustering exploration

# Choose number of clusters for test set

k0 = 5

# Choose centers as k0 x 2 matrix
centers = t(matrix(c(1,1,2,3,4,2,0,-1,-1,-1),nrow=2))

# set up data frame
# N points per cluster 
# we picked N = 100

N = 100

# First make a vector of cluster labels

cluster = c()

for (j in 1:5){
  cluster = c(cluster,rep(j,N))
}

# Next, Make vectors of x and y coordinates.
# There will be  N  points each clustered around each of the cluster centers.
# Choose sig to adjust the Variability around each cluster center.
# First we chose sig = 1/3, then we chose sig = 0.7.

sig = 0.7
x <- centers[cluster,1] + rnorm(500)*sig
y <- centers[cluster,2] + rnorm(500)*sig

# We can now plot all points, With colors depending on the cluster.

plot(x,y, col = cluster)

# We can also plot the centers of the "True" clusters, using thick x's.

points(centers[,1], centers[,2], col = 1:5, pch = 4,lwd=3)

# Make a data frame to work with.

mydf = data.frame(truecluster = cluster, x=x, y=y)

# Guess some centroids.

centroids = matrix(rnorm(10)*2,nrow=5) 

# make column in the data frame
# which will contain the cluster numbers during the iteration.

mydf$clusterguess = rep(0,5*N)

# For each point, find the closest centroid and write the label of that centroid in the last column

for (j in 1:(5*N)){
  xy = c(mydf$x[j], mydf$y[j])
  dist = rowSums((centroids - matrix(rep(xy,each=5),nrow=5))^2)
  mydf$clusterguess[j] = which(dist == min(dist))
}

# Plot all points together with the guessed centroids (Thick diamonds). 

plot(mydf$x, mydf$y,col = mydf$clusterguess)
points(centroids[,1],centroids[,2],col=1:5, pch=5, lwd=3)

# We can check how many points are in each of the guessed clusters at this point.

table(mydf$clusterguess)

# recompute centroids

for (j in 1:5){
  centroids[j,1] <- mean(mydf$x[ mydf$clusterguess == j])
  centroids[j,2] <- mean(mydf$y[ mydf$clusterguess == j])
}

# We can now plot the recomputed cluster centers (thick triangles)

points(centroids[,1],centroids[,2],col=1:5, pch=6, lwd=3)


# recompute cluster gesses and plot

# repeated until it stabilizes!

#########  Extracting terminal to terminal traffic matrix using table command

# load Q2_13
# Change names to lowercase

names(Q2_13) <- tolower(names(Q2_13))
Q2_13$ones <- 1 # Not actually needed here

# make table of all start /end ride combinations

T = table(Q2_13$start.terminal, Q2_13$end.terminal)

# T is a 2 way table. R treats it like a matrix with row and column names