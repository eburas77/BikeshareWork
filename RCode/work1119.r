# Aggregate over all combinations of day / subscription type / hour / start station.
# Use the df Q2 from work1111.Rdata to do this.

MyDF = aggregate(date ~ day*subscription.type*hour*start.terminal, data = Q2, FUN = length)
names(MyDF)[5] <- "count"  # Since the argument "date" is arbitrary and this column really conatins a count
MyDF$hour <- as.numeric(MyDF$hour) 

table(MyDF$day, MyDF$subscription.type)

############ Multiple Clustering and Plotting

# Import xml data and turn into data frame
# Need to install package XML first 

library("XML")

mystations <- xmlToDataFrame(doc = "http://www.capitalbikeshare.com/data/stations/bikeStations.xml")

# Convert latitudes and longitudes into numbers

mystations$long <- as.numeric(as.character(mystations$long))
mystations$lat <- as.numeric(as.character(mystations$lat))

# Make data frame of counts per combination of hour and station
# Set number of clusters

nclust = 4

#########  Prepare data frame MyDF

# >>>>>> Can use subsets of Q2 to modify this
# >>>>>> For example, delete rides that start or end at statiosn with low overall activity
# >>>>>> Or use only rides on weekdays or weekends or only by subscribers etc.

MyDF = aggregate(date ~ hour*start.terminal, data = Q2, FUN = length)
names(MyDF)[3] <- "count"  # Since the argument "date" is arbitrary and this column really conatins a count
MyDF$hour <- as.numeric(MyDF$hour) 

# Make a data frame for cluster memberships.
# One column contains terminal numbers,
# the other nclust columns contain membership probabilities.
# Also make names for the columns.

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

# The following code makes multiple cluster plots
# Clusters are plotted in black - red - green - blue
# depending on oveall activity level

plotcount = 20   # set the number of plots to be made

plottitle = "bikeplot.pdf"   # change as needed
pdf(file = plottitle)

for (k in 1:plotcount){
  mypi <- rep(1,nclust)/nclust
  
  lambdas[,2:(1+nclust)] <- 50*runif(nclust*24)
  
  ###########  Now run EM, 20 iterations
  
  for (j in 1:20){
    
    #########  E Step
    
    for (station in clustermem$start.terminal){
      MyDF0 <- MyDF[MyDF$start.terminal == station,] # Extract all counts for this station (24 rows)
      MyDF0 <- MyDF0[order(MyDF0$hour), ] # Order by hour
      result <- clusterprob(MyDF0$count,as.matrix(lambdas[,2:(1+nclust)]), mypi)
      clustermem[clustermem$start.terminal == station, 2:(1+nclust)] <- result$cprobs
    }
    
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
  
  # Plot, using a different color for each cluster, depending on overall activity = mean of lambdas
  
  plotcolors = (1+nclust) - rank(colMeans(lambdas[,2:(1+nclust)]))
  
  
  plot(clustersummary$long, clustersummary$lat, xlab = "Longitude",ylab = "Latitude", pch = 46,
       main = paste("Clusters of DC  Bike Stations") )
  for (j in 1:nclust){
    select <- clustersummary$cluster == cnames[j]
    points(clustersummary$long[select], clustersummary$lat[select], lwd = 2,col = plotcolors[j])
  }
  grid(col = 1)
  
}
dev.off()

