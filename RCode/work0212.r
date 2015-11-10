# MasterQ is the df we are woprking with
#
# Make time column
# Could be hour or something coarser

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

# Write function for M step
# Should take Xit. or Xijt. and Zil as arguments
# Uses computed valued values of alpha_i and T and D
# Computes lambdas and myp

Mstep1 <- function(MyX, MyZ){
  myp <- colMeans(MyZ)
  lambdas <- t(MyX)%*%MyZ
  lambdas <- lambdas / matrix(rep(colMeans(lambdas), each = T), nrow = T) 
  result <- list(lambdas = ambdas, myp = myp)
  return(result)
}

# Write function for E step
# Should take lambdas and myp and Xit. or Xijt. and Zil as arguments
# Computes Zil (which becomes MyZ)
