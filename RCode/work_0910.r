Q2_13 <- read.csv("http://www.capitalbikeshare.com/assets/files/trip-history-data/2013-2nd-quarter.csv")

mylist = list(1,pi/2,"abcde")
mode(mylist)
class(mylist)

mylist = list(x = c(1,2,3), y = pi/2,text = "abcde")

mylist[[3]]

mylist$text

mylist$x[3]
mylist[[1]][3]

class(mylist[[1]])
mode(mylist[[1]])
length(mylist$x)


myduration <- " 1h 35m 24sec "

Z <- gregexpr("[[:digit:]]+", myduration)

# this is a structure. What is that?

Z1 <- regmatches(myduration,Z)

as.numeric(unlist(Z1))

duration2numeric = function(myduration){
  Z <- gregexpr("[[:digit:]]+", myduration)
  Z1 <- regmatches(myduration,Z)
  Z2 <- as.numeric(unlist(Z1))
  x <- sum(Z2*c(3600,60,1))
  return(x)
}

durationchar <- as.character(Q2_13$Duration)

y = c()

for (j in 1:length(durationchar)){
  y <- c(y,duration2numeric(durationchar[j]))
}

Q2_13$durationsec <- y

Q2_13$Duration <- NULL

# Process an individual data frame:
# define function duration2numeric ONCE, then for each data frame:

durationchar <- as.character(Q2_13$Duration) # change name of data frame
                                             # change name of time record column if needed  
y = rep(0,length(durationchar))  # IMPORTANT
for (j in 1:length(durationchar)){
  y[j] <- duration2numeric(durationchar[j])
}

# visual check of results, use head()

Q2_13$durationsec <- y  # change name of data frame
Q2_13$Duration <- NULL  # change names

# clean up:

rm(y)
rm(durationchar)

