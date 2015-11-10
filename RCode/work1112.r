# Import xml data and turn into data frame
# Need to install package XML first 

mystations <- xmlToDataFrame(doc = "http://www.capitalbikeshare.com/data/stations/bikeStations.xml")

# How many stations are currently empty or full?

sum(mystations$nbBikes == 0)
sum(mystations$nbEmptyDocks == 0)

plot(mystations$long, mystations$lat) # does not work

mystations$long1 <- as.numeric(mystations$long)
mystations$lat1 <- as.numeric(mystations$lat)

# we'll get rid of these later. These are integers (essentially the ranks of the longitudes and latitudes) 
# plotting them gives a distorted picture, but it's recognizable. 

mystations$long <- as.numeric(as.character(mystations$long))
mystations$lat <- as.numeric(as.character(mystations$lat))

mystations$long1 <- mystations$lat1 <- NULL

plot(mystations$long, mystations$lat) # plots all current stations 

# plot stations with no bikes in red

select <- mystations$nbBikes == 0
points(mystations$long[select], mystations$lat[select], col = 2, lwd = 2)

# plot full stations in blue, use diamonds as plot symbol (pch = 5) 

select <- mystations$nbEmptyDocks == 0
points(mystations$long[select], mystations$lat[select], col = 4, lwd = 2, pch = 5)
grid(col = 1)

# Make a windowed plot

mylong = c(-77.08, -76.98)
mylat <- c(38.86, 38.96)

windowselect <- mystations$long > mylong[1]
windowselect <- windowselect & (mystations$long < mylong[2])
windowselect <- windowselect & (mystations$lat > mylat[1])
windowselect <- windowselect & (mystations$lat < mylat[2])

mystationselect <- mystations[windowselect,]

mymain = paste("Window, longitude", mylong, "latitude", mylat)

plot(mystationselect$long, mystationselect$lat, main = mymain) 

select <- mystationselect$nbBikes == 0
points(mystationselect$long[select], mystationselect$lat[select], col = 2, lwd = 2)

select <- mystationselect$nbEmptyDocks == 0
points(mystationselect$long[select], mystationselect$lat[select], col = 4, lwd = 2, pch = 5)
grid(col = 1)

################  CLUSTERING  ##############

########### First run the following function code to define clusterprob()

clusterprob = function(x,Lam,mypi){
  k <- length(mypi)
  logprobs <- cprobs <- rep(0,k)
  for (j in 1:k){
    logprobs[j] <- sum(dpois(x,Lam[,j],log=T))
  }
  cprobs = logprobs -  max(logprobs)
  cprobs <- pmax(cprobs, rep(-36,k))
  cprobs <- mypi*exp(cprobs)
  cprobs <- cprobs/sum(cprobs)  
  return(list(cprobs = cprobs,logprobs = logprobs))
}

#  Arbitrary number of clusters, cluster weekdays. This is material from previous week.

nclust = 4

#########  Prepare data frame MyDF

MyDF = aggregate(start.station ~ hour*date, data = Q2_13, FUN = length)
names(MyDF)[3] <- "count"  # Since the argument "start.station" is arbitrary
MyDF$hour <- as.numeric(MyDF$hour) 
select <- MyDF$date == "7/1/2013" # Return dates on 7/1. There were 24 such events.
MyDF[select,]   # He we see when these returns happened
MyDF <- MyDF[!select,]  # Remove these events from the data frame.

# Make a data frame for cluster memberships.
# One column contains dates,
# the other nclust columns contain membership probabilities.

clustermem <- aggregate(count ~ date, data = MyDF, FUN = length)
clustermem[,3:(2+nclust)] <- 0

cnames <- c()

for (j in 1:nclust){
  cnames <- c(cnames, paste("c",j,sep =""))
}
clustermem <- clustermem[,-2] # removes extraneous column 2
names(clustermem) <- c("date",cnames)

# Make a data frame for Poisson probabilities.
# One column contains hours, the others lambdas (one for each cluster).

lambdas <- aggregate(count ~ hour, data = MyDF, FUN = length)
lambdas[,3:(2+nclust)] <- 0
lambdas <- lambdas[,-2]
names(lambdas) <- c("hour",cnames)

# Make a vector of cluster probabilities and initialize it

mypi <- rep(1,nclust)/nclust

# setup is complete

# initialize lambdas

lambdas[,2:(1+nclust)] <- 350*runif(nclust*24)

###########  Then run EM

for (j in 1:20){
  
  #########  E Step
  
  for (day in clustermem$date){
    MyDF0 <- MyDF[MyDF$date == day,] # Extract all counts for this day (24 rows)
    MyDF0 <- MyDF0[order(MyDF0$hour), ] # Order by hour
    result <- clusterprob(MyDF0$count,as.matrix(lambdas[,2:(1+nclust)]), mypi)
    clustermem[clustermem$date == day, 2:(1+nclust)] <- result$cprobs
  }

# Plot hourly patterns after each iteration.
# This can be commented out.  
  
  plot(lambdas$c1, lwd = 3, ylim = c(0,1000), main = paste("Iteration",j))
  for (k in 3:(1+nclust)){
    points(lambdas[,k], col = k, lwd=3)
  }
  grid(col = 1)
  
  ##########  M Step
  
  mypi = colSums(as.matrix(clustermem[,2:(1+nclust)]))
  mypi <- mypi/sum(mypi)
  MyDFaux <- MyDF
  MyDFaux <- merge(MyDFaux, clustermem, by = "date")
  for (k in 4:(3+nclust)){
    MyDFaux[,k] <- MyDFaux[,k]*MyDFaux$count  
  }
  
  for (k in 1:nclust){
    lam1 <- aggregate(eval(parse(text=cnames[k])) ~ hour, data = MyDFaux, FUN = sum)
    lambdas[,k+1] <- lam1[,2]/sum(clustermem[,k+1])  
  }
  #######  End of M Step
}


#  Now we cluster stations (this was done on 11/12)

nclust = 3

#########  Prepare data frame MyDF

MyDF = aggregate(date ~ hour*start.terminal, data = Q2_13, FUN = length)
names(MyDF)[3] <- "count"  # Since the argument "date" is arbitrary and this column really conatins a count
MyDF$hour <- as.numeric(MyDF$hour) 

# Make a data frame for cluster memberships.
# One column contains terminal numbers,
# the other nclust columns contain membership probabilities.

clustermem <- aggregate(count ~ start.terminal, data = MyDF, FUN = length)
clustermem[,3:(2+nclust)] <- 0

cnames <- c()

for (j in 1:nclust){
  cnames <- c(cnames, paste("c",j,sep =""))
}
clustermem <- clustermem[,-2] # removes extraneous column 2
names(clustermem) <- c("start.terminal",cnames)

# Make a data frame for Poisson probabilities.
# One column contains hours (0 .. 23), the others lambdas (one for each cluster).

lambdas <- aggregate(count ~ hour, data = MyDF, FUN = length)
lambdas[,3:(2+nclust)] <- 0
lambdas <- lambdas[,-2]
names(lambdas) <- c("hour",cnames)

# Initialize cluster probabilities and lambdas

mypi <- rep(1,nclust)/nclust

# initialize lambdas

lambdas[,2:(1+nclust)] <- 350*runif(nclust*24)

###########  Now run EM

for (j in 1:20){
  
  #########  E Step
  
  for (station in clustermem$start.terminal){
    MyDF0 <- MyDF[MyDF$start.terminal == station,] # Extract all counts for this station (24 rows)
    MyDF0 <- MyDF0[order(MyDF0$hour), ] # Order by hour
    result <- clusterprob(MyDF0$count,as.matrix(lambdas[,2:(1+nclust)]), mypi)
    clustermem[clustermem$start.terminal == station, 2:(1+nclust)] <- result$cprobs
  }
  
#   plot(lambdas$c1, lwd = 3, ylim = c(0,1000), main = paste("Iteration",j))
#   for (k in 3:(1+nclust)){
#     points(lambdas[,k], col = k, lwd=3)
#  }
#  grid(col = 1)

# Uncomment this to see plots after each iteration  
  
  ##########  M Step
  
  mypi = colSums(as.matrix(clustermem[,2:(1+nclust)]))
  mypi <- mypi/sum(mypi)
  MyDFaux <- MyDF
  MyDFaux <- merge(MyDFaux, clustermem, by = "start.terminal")
  for (k in 4:(3+nclust)){
    MyDFaux[,k] <- MyDFaux[,k]*MyDFaux$count  
  }
  
  for (k in 1:nclust){
    lam1 <- aggregate(eval(parse(text=cnames[k])) ~ hour, data = MyDFaux, FUN = sum)
    lambdas[,k+1] <- lam1[,2]/sum(clustermem[,k+1])  
  }
  #######  End of M Step
}

# Summarize cluster membership

clustersummary <- clustermem
clustersummary[,2:(1+nclust)] <- round(clustersummary[,2:(1+nclust)])
clustersummary$cluster <- NA

for (j in 1:nclust){
  select <- clustersummary[,j+1] == 1
  clustersummary$cluster[select] <- names(clustersummary)[j+1]
}

for (j in 1:nclust){
  clustersummary[,2] <- NULL
}

# Now clustersummary has two columns, one with terminal numbers, one with cluster membership

# Make data frame with only terminal numbers, longitudes, and latitudes 

plotstations <- data.frame(start.terminal = mystations$terminalName, long = mystations$long, lat = mystations$lat)

# Make sure terminal numbers are no longer factors

plotstations$start.terminal <- as.numeric(as.character(plotstations$start.terminal))
clustersummary$start.terminal <- as.numeric(as.character(clustersummary$start.terminal))

# Merge

clustersummary <- merge(clustersummary, plotstations, by = "start.terminal")

# Now cluster summary has terminal number, latitude, longitude, and cluster info in its columns 

# Plot, using a different color for each cluster

pdf()
plot(clustersummary$long, clustersummary$lat, xlab = "Longitude",ylab = "Latitude",main = "Clustering Bike Stations" )
for (j in 1:nclust){
  select <- clustersummary$cluster == cnames[j]
  points(clustersummary$long[select], clustersummary$lat[select], lwd = 2, col = j)
}
grid(col = 1)

dev.off()