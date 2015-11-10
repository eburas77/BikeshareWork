# Write function for E step
# Should take lambdas and myp and Xit. or Xijt. and Zil as arguments
# Computes Zil (which becomes MyZ)

# changed on 2/19

# first attempt


n <- 5
myp <- runif(5)/sum(runif(5))
lambdas <- 2*matrix(runif(n*24),nrow=n)
y <- log(lambdas)%*%t(Xit.)
logziltilde <- t(log(myp) + y)
for (i in 1:233){
logziltilde[i,] <- logziltilde[i,] - max(logziltilde[i,])
logziltilde[i,logziltilde[i,] < -500] <- -500 
}
ziltilde <- exp(logziltilde)
zil <- ziltilde/rowSums(ziltilde)
# need to write up how we deal with the underflow







#junk that was first attempted
ziltilde <- matrix(0,233,n)
for (i in 1:233){
  a <- myp
  for (t in 1:24){
    a <- a*lambdas[,t]^Xit.[i,t]
  }
  ziltilde[i,]<-a
}



n <- length(Xit.)
a <- rep(1,n) #initialize vectors
b <- rep(1,233)
c <- rep(1,233)
Estep1 <- function(Xit., myp, lambdas){
  for (j in 1:n){
    a[j] <- (lambda[j,1]^Xit.[j,1]) #create a vector of the exponentials
  }
  for (k in 1:233){
    b[k] <- a[k]*a[k+24]
    c[k] <- myp[k]*b[k] 
    for (l in 1:232)
      d <- sum(c)
    zil[k] <- c[k]/d
    # first compute the product over t then multiply by the myp's then divide each entry by the rowSum
    
  }  
}