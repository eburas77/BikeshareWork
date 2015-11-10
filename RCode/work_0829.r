
# change all column names to lower case

names(Q1_11) <- tolower(names(Q1_11))
names(Q2_13) <- tolower(names(Q2_13))

# check if names agree or not

unique(c(names(Q1_11),names(Q2_13)))

# some names have changed! Some new columns have appeared!

# Try to compute duration.

myduration <- Q2_13$duration[100000] 

Q2_13[100000,]

class(Q2_13$duration)

myduration <- as.character(myduration)

thisduration = as.numeric(substr(myduration,4,4))*60 + as.numeric(substr(myduration,7,8))

# Try with another trip

myduration <- Q2_13$duration[200000] 

myduration <- as.character(myduration)

# This code will give the wrong result!

thisduration = as.numeric(substr(myduration,4,4))*60 + as.numeric(substr(myduration,7,8))


# Try this instead

n <- nchar(myduration)

mydurationpieces <- c()
for (j in 1:n){
  mydurationpieces <- c(mydurationpieces,substr(myduration,j,j))
}

# turn pieces into numbers

piecesnumeric <- as.numeric(mydurationpieces)

# identify pieces that are not NA

notna <- !is.na(piecesnumeric)

# use this to compute duration in seconds.

# histogram of Q2_12 data by bike key

hist(log10(Q2_12$Duration..sec.[Q2_12$Bike.Key =="Registered"]),breaks=100)

hist(log10(Q2_12$Duration..sec.[Q2_12$Bike.Key =="Casual"]),breaks=100)