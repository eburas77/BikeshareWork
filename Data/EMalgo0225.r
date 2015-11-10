ptm <- proc.time()      #start timer

# MasterQ is the df we are woprking with
#
# Make time column
# Could be hour or something coarser
MasterQ <- Q2

MasterQ$time <- as.numeric(MasterQ$hour)

# Compute X_it. array
# Should be flexible enough to make also X_ijt. array

Xijt. <- table(MasterQ$start.terminal, MasterQ$end.terminal,MasterQ$time)
Xit. <- table(MasterQ$start.terminal, MasterQ$time)
Xjt. <- table(MasterQ$end.terminal,MasterQ$time)

T <- length(unique(MasterQ$time))
D <- length(unique(MasterQ$date))

# first index  i is start, second index j is end, third index is time 
# Can access it as Xijt.["31001", "31000", "0"]

# Make alpha_i

alpha_i <- rowSums(Xit.)/T/D 
# where do we use this!!!!! do we not need it to calculate the lambdas?
# did we work around the alphas in the matrix algebra in the m step?

# Write function for M step
# Should take Xit. or Xijt. and Zil as arguments
# Uses computed valued values of alpha_i and T and D
# Computes lambdas and myp

n <- 5 # how many clusters do you want
myp <- runif(5)/sum(runif(5))  # initialize myp
lambdas <- 2*matrix(runif(n*24),nrow=n) # initialize the lambdas

# E step function to calculate zil (cluster probabilities) based on myp, Xit. and lambdas
Estep1 <- function(myp, Xit., lambdas){
y <- log(lambdas)%*%t(Xit.)
logziltilde <- t(log(myp) + y)
for (i in 1:233){
  logziltilde[i,] <- logziltilde[i,] - max(logziltilde[i,])
  logziltilde[i,logziltilde[i,] < -500] <- -500 # deals with underflow
}
ziltilde <- exp(logziltilde)
zil <- ziltilde/rowSums(ziltilde)
}

# M step function to calculate parameters (myp and lambdas) based on Xit. and newly computed zil
# previously in work0212.r we used MyX and MyZ but if I am not mistaken, these values are now
# Xit. and zil to run in the iterative algorithm

Mstep1 <- function(Xit., zil){
  myp <- colMeans(zil)
  lambdas <- t(Xit.)%*%zil
  lambdas <- lambdas / matrix(rep(colMeans(lambdas), each = T), nrow = T)
  lambdas <- t(lambdas) # found that lambdas must be transposed to re-enter E step
}

# EM algorithm

for (j in 1:20){
  Estep1(myp, Xit., lambdas)
  Mstep1(Xit., zil)
}

clustersummary <- data.frame(zil)
clustersummary <- round(clustersummary)
# clustersummary now is a 233*5 data frame with cluster memberships for unnamed clusters

proc.time() - ptm            #end timer