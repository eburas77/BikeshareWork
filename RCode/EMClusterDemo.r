# EM Cluster demo

# Install and load the EMCluster package. 

set.seed(1234)

x <- da1$da # loads an example  data frame 

TC <- da1$class # these are the true classes 

n <- nrow(x)
p <- ncol(x)

k <- 10  # or any other number of clusters

ret.em <- init.EM(x, nclass = k, method = "em.EM") #initialize

ret.Rnd <- init.EM(x, nclass = k, method = "Rnd.EM", EMC = .EMC.Rnd) # initialize

ret.Rndp <- init.EM(x, nclass = k, method = "Rnd.EM", EMC = .EMC.Rndp) # initialize

ret.svd <- emgroup(x, nclass = k) # this a plot of the first  

par(mfrow = c(2, 2)) # prepares for 2x2 subplots

plotem(ret.em, x, main = "em") # first method 

plotem(ret.Rnd, x, main = "Rnd") # second method

plotem(ret.Rndp, x, main = "Rnd+") # third method

plotem(ret.svd, x, main = "svd") # foruth method

# make a matrix for numerical output of diagnostic quantities

ret.all <-
   cbind(
       c(ret.em$llhdval, ret.Rnd$llhdval, ret.Rndp$llhdval,
               ret.svd$llhdval),
       c(RRand(ret.em$class, TC)$adjRand,
               RRand(ret.Rnd$class, TC)$adjRand,
               RRand(ret.Rndp$class, TC)$adjRand,
               RRand(ret.svd$class, TC)$adjRand)
     )

rownames(ret.all) <- c("em", "Rnd", "Rnd+", "svd")
colnames(ret.all) <- c("logL", "adjR")

# display:

ret.all