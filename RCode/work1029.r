# Prepare for clustering of weekdays
# Aggregate counts of returns by hour and date:

#########  Prepare data frame MyDF

MyDF = aggregate(start.station ~ hour*date, data = Q2_13, FUN = length)
names(MyDF)[3] <- "count"  # Since the argument "start.station" is arbitrary
MyDF$hour <- as.numeric(MyDF$hour) 
select <- MyDF$date == "7/1/2013" # Return dates on 7/1. There were 24 such events.
MyDF[select,]   # He we see when these returns happened
MyDF <- MyDF[!select,]  # Remove these events from the data frame.

# Make a data frame for custom memberships.
# One column contains dates,
# the other two contain membership probabilities.

clustermem <- aggregate(count ~ date, data = MyDF, FUN = length)
clustermem$c1 <- rep(0,91)
clustermem$c2 <- rep(0,91)
clustermem <- clustermem[,-2] # removes extraneous column 2

# Make a data frame for Poisson probabilities.
# One column contains hours, the other two contain lambdas (one for each cluster).

lambdas <- aggregate(count ~ hour, data = MyDF, FUN = length)
lambdas$c1 <- rep(0,24)
lambdas$c2 <- rep(0,24)
lambdas <- lambdas[,-2]

# Make a vector of cluster probabilities and initialize it

mypi <- c(.5,.5) 

# setup is complete

# initialize lambdas

lambdas$c1 <- 10*runif(24)*2 # Completely random
lambdas$c2 <- 10*runif(24)*2 # Also completely random

# Compute prob of single day pattern for cluster 1

########### Next run the function code at the bottom to define clusterprob()

###########  Then run EM 100 times with 2 clusters

for (j in 1:100){

#########  E Step

for (day in clustermem$date){
  MyDF0 <- MyDF[MyDF$date == day,] # Extract all counts for this day (24 rows)
  MyDF0 <- MyDF0[order(MyDF0$hour), ] # Order by hour
  result <- clusterprob(MyDF0$count,as.matrix(lambdas[,2:3]), mypi)
  clustermem[clustermem$date == day, 2:3] <- result$cprobs
}


##########  M Step

mypi = colSums(as.matrix(clustermem[,2:3]))
mypi <- mypi/sum(mypi)
MyDFaux <- MyDF
MyDFaux <- merge(MyDFaux, clustermem, by = "date")
MyDFaux$c1 <- MyDFaux$c1*MyDFaux$count
MyDFaux$c2 <- MyDFaux$c2*MyDFaux$count

lam1 <- aggregate(c1 ~ hour, data = MyDFaux, FUN = sum)
lambdas$c1 <- lam1$c1/sum(clustermem$c1)

lam2 <- aggregate(c2 ~ hour, data = MyDFaux, FUN = sum)
lambdas$c2 <- lam2$c2/sum(clustermem$c2)

#######  End of M Step
}

########### Plot lambdas

plot(lambdas$c1, lwd = 3)
points(lambdas$c2, col = 2, lwd=3)
grid(col = 1)

########### Identify dates in cluster  1 

clustermem$date[clustermem$c1 > .9]


# function to estimate cluster probabilities for count data
# Designed to avoid underflow
# Input:  x  -- count vector with  n  entries
#         Lam  --   n x k matrix of Poisson parameters
#                   column  j  has parameters for cluster j
#         mypi  --  k-vector of cluster probabilities
#                   mupi[j] = prob(cluster j)
#
# Output: list with members 
#     $logprobs  --   k-vector with log probbilities Prob(x|lambda = Lam[,k])
#                     assuming independence of counts in x
#     $cprobs  -- k-vector of probabilities of cluster membership

clusterprob = function(x,Lam,mypi){
  k <- length(mypi)
  logprobs <- cprobs <- rep(0,k)
  for (j in 1:k){
    logprobs[j] <- sum(dpois(x,Lam[,j],log=T))
  }
  cprobs = logprobs -  max(logprobs)
  cprobs <- pmax(cprobs, rep(-36,k))
  cprobs <- mypi*exp(cprobs)
  cprobs <- cprobs/sum(cprobs)  
  return(list(cprobs = cprobs,logprobs = logprobs))
}
