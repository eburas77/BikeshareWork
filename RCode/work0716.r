# Generate monthly lambda array monthly_lambdas_r_clusters
# and monthly cluster prob array  monthly_myp_r_clusters
# r = number of clusters
# The lambda array is a T x r x 43 array
# monthly_lambdas_r_clusters[,,1] contains lambdas from years 2012 and 2013
# monthly_lambdas_r_clusters[,,2:43] contains monthly lambdas from Oct '10 to Mar '14
# The myp array is a nclust x 43 array
# monthly_myp_r_clusters[,1] contains myp from years 2012 and 2013
# monthly_myp_r_clusters[,2:43] contains monthly myp's from Oct '10 to Mar '14

# select subscriber / casual
# select weekday / weekend

select0 <- Q.all$wday == "wday" & Q.all$subscription.type == "Subscriber"

nclust = 5

# Make array that will hold the lambdas

monthly_lambdas_clusters <- array(0,dim = c(nclust,24,43))
monthly_myp_clusters <- array(0,dim = c(nclust,43))

select <- select0 & (Q.all$year == 2013 | Q.all$year == 2012)

Q <- Q.all[select,]

Qinit <- EMinitialize(Q,nclust)
mypinit <- Qinit$myp
mylambdasinit <- Qinit$lambdas

Z <- runEM2(Q, nclust, mypinit, mylambdasinit, steps = 500)

# Sort resulting lambdas and cluster probabilities, those with earliest mean come first
# Assign to appropriate layer in 
# monthly_lambdas_r_clusters and monthly_myp_r_clusters
# These will now be used to initialize monthly runs

plotmylambdas(Z$lambdas)
monthly_lambdas_clusters[,,1] <- Z$lambdas[order(colMeans(t(Z$lambdas)*(0:23))),]
monthly_myp_clusters[,1] <- Z$myp[order(colMeans(t(Z$lambdas)*(0:23)))]

# Compute monthly lambdas and myp's 
# Use values for 2012 and 2013 as initial values

mylambdasinit <- monthly_lambdas_clusters[,,1] 
mypinit <- monthly_myp_clusters[,1] 

for (j in 2:43){
  select <- select0 & Q.all$monthcount == j
  Q <- Q.all[select,]
  Z <- runEM2(Q, nclust, mypinit, mylambdasinit, steps = 100)
  monthly_lambdas_clusters[,,j] <- Z$lambdas 
  monthly_myp_clusters[,j] <- Z$myp 
  print(paste("month  ", j, " is complete"))
}

pdf(file = "lambdas_by_month.pdf")
for (j in (2:43)){
  mymain = paste("lambdas for month ",j)
  plotmylambdas(monthly_lambdas_clusters[,,j],mymain)
}
dev.off()

pdf(file = "difflambdas_by_month.pdf")
for (j in (2:43)){
  mymain = paste("lambda anomalies for month ",j)
  plotmylambdas(monthly_lambdas_clusters[,,j] - monthly_lambdas_clusters[,,1],mymain)
}
dev.off()

