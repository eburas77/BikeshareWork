# Convert times to POSIXct

Q2_13$stime <- strptime(Q2_13$start.time,"%m/%d/%Y %H:%M")
Q2_13$etime <- strptime(Q2_13$end.date,"%m/%d/%Y %H:%M")

# Extract weekdays, e.g.

weekdays(Q2_13$stime[1])

# Separate date and hour of arrival
# Need to source the function defined at the bottom


dateAndTime = date_time(Q2_13$end.date)
Q2_13$date = dateAndTime[,1]
Q2_13$time = dateAndTime[,2]
Q2_13$hour = t(matrix(unlist(strsplit(Q2_13$time,":")), nrow=2))[,1]
Q2_13$weekday = weekdays(Q2_13$etime)

# Make table of number of rides by weekday, hour, subscriber type

mytable = table(Q2_13$weekday,Q2_13$hour,Q2_13$subscription.type)

wd <- c(5,1,6,7,4,2,3)
names(wd) <- rownames(mytable)
hr <- as.numeric(colnames(mytable))
names(hr) <- colnames(mytable)

# Make df with number of rides for each week hour, 
# for each subscriber type

mydf <- data.frame(hour = rep(0,168), casual = rep(0,168), subsc = rep(0,168))

for (j in 1:7){
  for (k in 1:24){
    r =  hr[k] + 24*(wd[j]-1)
    mydf[r+1,] = c(r,mytable[j,k,1]/13,mytable[j,k,2]/13)
  }
}

# Make plot, with some bells and whistles.
# This code does not put on labels and tickmarks.

myxlab = "Hours after Monday 0:00"
myylab = "Riders"
mymain = "Hourly Ridership, April - June 2013"
mysub = "Subscribers in red, casual riders in green"
plot(mydf$hour,
     mydf$subsc,
     labels=FALSE,
     tick = FALSE,
     type='l',
     col=2,
     lwd=3,
     xlim=c(0,168),
     xlab = myxlab, 
     ylab = myylab, 
     main = mymain 
     # ,sub = mysub
     )

# Plot also casual riders

lines(mydf$hour,
       mydf$casual,
       col=3,
       lwd=3)

# Make grid lines

for (j in 0:7){abline(v=j*24,lwd=2)} 
for (j in 0:28){abline(v=j*6,lwd=.5,col=8)}

for (j in 0:8){abline(h=j*200,col=8)}

# Make labels and tickmarks for weekdays (see help)!

# Put a legend in the upper left corner (e.g.) to explain colors! 


date_time = function(mytime){
  # separates start / end time from csv file into date and time
  # input: vector mytime with entries in the format "mm/dd/yyyy hh:mm"
  # mytime is a vector of levels  or a character vector
  # mm can be 1 or 2 characters
  # dd can be 1 or 2 characters
  # yyyy is 4 characters 
  # hh is 1 or 2 characters
  # mm is 2 characters
  #
  # output will be a char matrix with columns
  #   column 1 is character vector with dates
  #   column 2 is character vector with times
  mytime = as.character(mytime)
  result = t(matrix(unlist(strsplit(mytime," ")), nrow=2))
  return(result)
}
