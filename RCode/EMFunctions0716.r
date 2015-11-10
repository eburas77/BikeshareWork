runEM2 = function(Q, nclust, mypinit, mylambdasinit, steps = 100){
  # make count table
  Xijt. <- table(Q$start.terminal, Q$end.terminal, Q$hour)
  
  # Find T and D and initialize

  T <- length(unique(Q$hour))
  D <- length(unique(Q$date))
  myp <- mypinit
  lambdas <- mylambdasinit
    
  # Run EM
    
  for (j in 1:steps){
    zijl <- Estep2(myp, Xijt., lambdas)
    out <- Mstep2(Xijt., zijl)
    myp <- out$myp
    lambdas <- out$lambdas
  }
  result = list(lambdas = lambdas, myp = myp)
  return(result)
}



################## Contents from EMfunction0618 are below this line 

summarize = function(Q){
  for (j in 1:dim(Q)[2]){
    tt <- table(Q[,j])
    if (length(tt) < 10){
      print(names(Q)[j])
      print(tt)
    }
  }
}

# Initialize for EM
# Input: 
# Q = data frame with ride data,
# must have columns $hour and $date
# nclust = Number of clusters
# 
# Output = a list With members
# T = number of hours in a day
# D = Number of days in the period under consideration
# myp = Cluster probabilities (Random)
# lambdas = Matrix of land patterns (random)

EMinitialize = function(Q, nclust){
  T <- length(unique(Q$hour))
  D <- length(unique(Q$date))
  myp <- runif(nclust)
  myp <- myp/sum(myp)
  lambdas <- 2*matrix(runif(nclust*T),nrow = nclust) # initialize the lambdas
  result <- list(T = T, D = D, myp = myp, lambdas = lambdas)
  return(result)
}

# Make a table for clustering stations
# The result is a station x hour table

makeTable1 = function(countdata, dim1, tdim){
  A <- table(countdata[,dim1], countdata[,tdim])
  return(A)
}

# Make a table for clustering station pairs
# The result is a station x station x hour cable

makeTable2 = function(countdata, dim1, dim2, tdim){
  A <- table(countdata[,dim1], countdata[,dim2], countdata[,tdim])
  return(A)
}


# Expectation step for clustering stations:
#
# Input 
#
# myp = vector of cluster probabilities
# Xit. = station x hour  array of count data
# lambdas = cluster x hour array of Lambda estimates
#
# output
#
# zil = Station x Cluster array of cluster probabilities
#

Estep1 <- function(myp, Xit., lambdas){
  y <- log(lambdas)%*%t(Xit.)
  logziltilde <- t(log(myp) + y)
  for (i in 1:length(Xit.[,1])){
    logziltilde[i,] <- logziltilde[i,] - max(logziltilde[i,])
    logziltilde[i,logziltilde[i,] < -500] <- -500 # deals with underflow
  }
  ziltilde <- exp(logziltilde)
  zil <- ziltilde/rowSums(ziltilde)
  return(zil)
}

# M step function for clustering stations
#
# input
#
# zil = Station x cluster array of cluster probabilities
# Xit. = Station x hour array of count data
#
# output
#
# list with members 
# $lambdas = Cluster x hour array of lambda estimates
# $myp = vector of cluster probabilities

Mstep1 <- function(Xit., zil){
  myp <- colMeans(zil)
  lambdas <- t(Xit.)%*%zil
  lambdas <- lambdas / matrix(rep(colMeans(lambdas), each = T), nrow = T)
  lambdas <- t(lambdas) # found that lambdas must be transposed to re-enter E step
  result <- list(myp=myp,lambdas=lambdas)
return(result)
}

# Expectation step for Clustering station pairs:
#
# Input 
#
# myp = vector of cluster probabilities
# Xijt. = start.station x end.station x hour array of count data
# lambdas = cluster x hour array of parameter estimates
#
# output
#
# zijl =  start.station x end.station x cluster array of cluster probabilities
#

Estep2 <- function(myp,Xijt.,lambdas){
  Xdim = dim(Xijt.)
  y <- matrix(Xijt., nrow = Xdim[1]*Xdim[2])%*%t(log(lambdas))
  logzijltilde <- t(t(y)+log(myp))
  matrixmax <- apply(logzijltilde,1,max)
  logzijltilde <- logzijltilde - matrixmax
  zijltilde <- exp(logzijltilde)
  zijl <- zijltilde/rowSums(zijltilde)
  zijl <- array(zijl,dim=c(Xdim[1],Xdim[2],length(myp)))
  return(zijl)
}

# M step function for clustering station pairs
#
# input
#
# zijl = start.station x end.station xcluster array of cluster probabilities
# Xijt. = start.station x end.station x hour array of count data
#
# output
#
# list with 
# $lambdas = cluster x hour array of parameter estimates
# $myp = vector of cluster probabilities

Mstep2 <- function(Xijt.,zijl){
  myp <- colMeans(zijl,dims=2)
  lambdas <- matrix(0,nrow=length(myp),ncol=T)
  for (ell in 1:length(myp)){
    for (t in 1:T){
      lambdas[ell,t] <- sum(Xijt.[,,t]*zijl[,,ell])  #flag
    }
    lambdas[ell,] <- lambdas[ell,]/mean(lambdas[ell,])
  }
  result <- list(myp=myp,lambdas=lambdas)
  return(result)
}

# Plot routine for lambdas
# Input: lambdas = lambda pattern (could also use  differences of lambdas)
# Optional input: Title of plot

plotmylambdas <- function(lambdas, mymain = "Lambdas vs. Time"){
  mylim <- c(min(lambdas),max(lambdas))
  plot(lambdas[1,],type='l',lwd=2,ylim=mylim, xlab = "time", ylab = "lambda",
       main = mymain)
  grid(col=1) 
  for (j in 2:dim(lambdas)[1]){
    lines(lambdas[j,],lwd=2, col=j)
  }
}

# Plot outbound arrows (not yet used)

outarrows = function(pickATerminal,cluster = 1:10,ridecount = 0){
  stationName <- as.character(mystations$name[mystations$terminalName == pickATerminal])  
  myterminal <- (1:233)[rownames(plotmatrix) == pickATerminal]  
  allrides <- sum(Xij..[myterminal,])  
  clust_rides <- sum(Xij..[myterminal,plotmatrix[myterminal,] > 0 ])
  
  mysub = paste(clust_rides," of ", allrides, " rides") 
    plot(mystations$long,mystations$lat, xlim=c(-77.12,-76.95),ylim=c(38.8,39), 
       main = stationName,
       sub = mysub,
       axes = T,
#       xlab = "longitude", ylab = "latitude",
       xlab = "", ylab = "",
      asp = .75)
  
  for (k in intersect(1:n,cluster)){
    c <- allNames[plotmatrix[myterminal,]==k]
    c <- c[c != pickATerminal]
    c <- intersect(c,colnames(Xij..)[Xij..[myterminal,] >= ridecount])    
      for (j in 1:length(c)){
        x0 <- mystations$long[mystations$terminalName == pickATerminal]
        y0 <- mystations$lat[mystations$terminalName == pickATerminal]
        x1 <- mystations$long[mystations$terminalName==c[j]]  
        y1 <- mystations$lat[mystations$terminalName==c[j]] 
        if (length(x1) > 0){
        arrows (x0, y0, x1, y1, length = .1, col = k, lwd = 2)
        }
      }
    }
}

# Plot inbound arrows (not yet used)

inarrows = function(pickATerminal,cluster = 1:10, ridecount = 0){
  stationName <- as.character(mystations$name[mystations$terminalName == pickATerminal])  
  myterminal <- (1:233)[rownames(plotmatrix) == pickATerminal]  
  allrides <- sum(Xij..[,myterminal])  
  clust_rides <- sum(Xij..[plotmatrix[,myterminal] > 0,myterminal ])

  mysub = paste(clust_rides," of ",allrides, " rides") 
  
  plot(mystations$long,mystations$lat, xlim=c(-77.12,-76.95),ylim=c(38.8,39), 
       main = stationName,
       sub = mysub,
#       xlab = "longitude", ylab = "latitude"
       xlab = "", ylab = "",
       asp = .75)
  
  for (k in intersect(1:n,cluster)){
    c <- allNames[plotmatrix[,myterminal]==k]
    c <- intersect(c,rownames(Xij..)[Xij..[,myterminal] >= ridecount])    
  #  c <- intersect(c,colnames(Xij..[,pickATerminal] >= ridecount))    
    if (length(c) > 0){
      for (j in 1:length(c)){
        x1 <- mystations$long[mystations$terminalName == pickATerminal]
        y1 <- mystations$lat[mystations$terminalName == pickATerminal]
        x0 <- mystations$long[mystations$terminalName==c[j]]  
        y0 <- mystations$lat[mystations$terminalName==c[j]] 
        if (length(x0) > 0){
        arrows (x0, y0, x1, y1, length = .1, col = k, lwd = 2)
        }
      }
    }
  }
}

