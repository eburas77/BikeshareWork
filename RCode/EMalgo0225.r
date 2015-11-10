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

source("EMfunctions.r")

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

n <- 4 # how many clusters do you want
myp <- runif(n)
myp <- myp/sum(myp)  # initialize myp
lambdas <- 2*matrix(runif(n*24),nrow=n) # initialize the lambdas

# EM algorithm

for (j in 1:100){
  zil <- Estep1(myp, Xit., lambdas)
  OUTPUT <- Mstep1(Xit., zil)
  myp <- OUTPUT$myp
  lambdas <- OUTPUT$lambdas
}

clustersummary <- data.frame(zil)
roundclustersummary <- round(clustersummary)
# clustersummary now is a 233*5 data frame with cluster memberships for unnamed clusters

numclusters <- colSums(round(zil))
plotlambdas <- lambdas[numclusters>0,]
mylim <- c(0,max(plotlambdas))
plot(plotlambdas[1,],type='l',lwd=2,ylim=mylim)
grid(col=1)
for (j in 2:sum(numclusters>0)){
  lines(plotlambdas[j,],lwd=2, col=j)
}

proc.time() - ptm            #end timer