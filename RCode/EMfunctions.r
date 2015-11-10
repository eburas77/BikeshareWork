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
  return(zil)
}

# M step function to calculate parameters (myp and lambdas) based on Xit. and newly computed zil
Mstep1 <- function(Xit., zil){
  myp <- colMeans(zil)
  lambdas <- t(Xit.)%*%zil
  lambdas <- lambdas / matrix(rep(colMeans(lambdas), each = T), nrow = T)
  lambdas <- t(lambdas) # found that lambdas must be transposed to re-enter E step
  result <- list(myp=myp,lambdas=lambdas)
return(result)
}

Estep2 <- function(myp,Xijt.,lambdas){
 

}

Mstep2 <- function(Xijt.,zijl){
  myp <- colMeans(zijl,dims=2)
  lambdas <- matrix(0,nrow=n,ncol=T)
  for (ell in 1:n){
    for (t in 1:T){
      lambdas[ell,t] <- Xijt.[,,t]*zijl[,,ell]  #flag
      }
    lambdas[ell,] <- [lambdas[ell,]/mean(lambdas[ell,])
  }
  result <- list(myp=myp,lambdas=lambdas)
return(result)
}




