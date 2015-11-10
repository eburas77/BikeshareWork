# Prepare for clustering of weekdays
# Aggregate counts of returns by hour and date:

MyDF = aggregate(start.station ~ hour*date, data = Q2_13, FUN = length)

names(MyDF)[3] <- "count"  # Since the argument "start.station" is arbitrary

class(MyDF$date) # This is currently character
class(MyDF$hour) # This is currently character
MyDF$hour <- as.numeric(MyDF$hour) # So turn this into numbers

select <- MyDF$date == "7/1/2013" # Return dates on 7/1. There were 24 such events.
MyDF[select,]   # He we see when these returns happened
MyDF <- MyDF[!select,]  # Remove these events from the data frame.

# Now the data frame has counts for 91 days times 24 hours.

sum(MyDF$count) + 24 - 749202 # Check that nothing has been overlooked

dpois(0:10, 3.2) # Compute Poisson probabilities for  x = 0, 1, ..., 10, for lambda = 3.2

ppois(0:10, 3.2) # This computes cumulative probabilities
rpois(10, 3.2) # This makes 10 random samples with this lambda

mylam1 <- c(3.2, 4.3, 2.1) 
myx <- c(4, 2, 3)
P1 <- dpois(myx,mylam1) # This computes a vector of three probabilities,
                        # The first one is P(x = 4) for lambda = 3.2, and so on      

prod(P1)  # This is the probability of seeing these three counts simultaneously
          # If counts are independent

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

clusterprob <- c(.5,.5) 

# setup is complete

# initialize lambdas

lambdas$c1 <- 343*runif(24)*2 # Completely random, but they have the right order of magnitude
lambdas$c2 <- 343*runif(24)*2 # Also completely random

# Compute prob of single day pattern for cluster 1

day = "4/1/2013"
MyDF0 <- MyDF[MyDF$date == day,] # Extract all counts for this day (24 rows)
MyDF0 <- MyDF0[order(MyDF0$hour), ] # Order by hour

P1 <- dpois(MyDF0$count, lambdas$c1, log = T) # Compute log probabilities for these counts,
                                              # assuming cluster 1
P2 <- dpois(MyDF0$count, lambdas$c2, log = T) # Same thing, assuming cluster 2

#########  To be used on 10/29. Play with this function.

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
  logprobs <- clusterprobs <- rep(0,k)
  for (j in 1:k){
    logprobs[j] <- sum(dpois(x,Lam[,j],log=T))
  }
  cprobs = logprobs -  max(logprobs)
  cprobs <- pmax(cprobs, rep(-36,k))
  cprobs <- mypi*exp(cprobs)
  cprobs <- cprobs/sum(cprobs)  
  return(list(cprobs = cprobs,logprobs = logprobs))
}
