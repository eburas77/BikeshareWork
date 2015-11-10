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

for (j in 1:150){
  zijl <- Estep2(myp, Xijt., lambdas)
  OUTPUT <- Mstep2(Xijt.,zijl)
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

plotmylambdas <- function(lambdas){
  mylim <- c(0,max(lambdas))
  plot(lambdas[1,],type='l',lwd=2,ylim=mylim)
  grid(col=1)
  for (j in 2:n){
    lines(lambdas[j,],lwd=2, col=j)
  }
}
plotmylambdas(lambdas)

}

# creating data frame with long lat and cluster
data <- as.data.frame(zil)
data$terminal <- row.names(zil)
newdf <- merge(mystations, data, by.x = "terminalName", by.y = "terminal")

plot(mystations$long,mystations$lat, xlim=c(-77.12,-76.95),ylim=c(38.8,39))

newdata$cluster <- 0

for (j in 1:n){
    select <- newdf[,j+5] > .5
    newdf$cluster[select] <- j
} # need to write something to plot these



#### arrows and plotting for station pairs


plotmatrix <- matrix(0, nrow=233, ncol=233)
row <- rownames(Xijt.)
col <- colnames(Xijt.)
rownames(plotmatrix) <- rownames(Xijt.)
colnames(plotmatrix) <- colnames(Xijt.)

for (i in 1:n){
  plotmatrix[zijl[,,i] > .99] <- i
}
table(plotmatrix)

# dupont circle number 81

table(plotmatrix[116,]) # 81 is the number in newdf
  
c <- col[plotmatrix[116,]==1]
d <- col[plotmatrix[116,]==2]
for (k in 1:n){
  c <- col[plotmatrix[116,]==k]
for (j in 1:length(c)){
  x0 <- mystations$long[116] 
  y0 <- mystations$lat[116]
  x1 <- mystations$long[mystations$terminalName==c[j]]  
  y1 <- mystations$lat[mystations$terminalName==c[j]] 
  arrows (x0, y0, x1, y1, length = .1, col = k, lwd = 2)
}}

x0 <- mystations$long[25] 
y0 <- mystations$lat[25]
x1 <- mystations$long[mystations$terminalName==c[2]]  
y1 <- mystations$lat[mystations$terminalName==c[2]]  




plot(mystations$long, mystations$lat)
x0 <- mystations$long[1]
y0 <- mystations$lat[1]
x1 <- mystations$long[100]
y1 <- mystations$lat[100]

arrows (x0, y0, x1, y1, length = .1, col = 2, lwd = 3)






cproc.time() - ptm            #end timer