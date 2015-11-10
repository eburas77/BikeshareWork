names(Q2_13) <- tolower(names(Q2_13))

# Make a column of ones in the main data frame

Q2_13$ones <- rep(1,length(Q2_13[,1]))

# Here is a way of finding out how many rides came out of each start terminal.
# The result is given as a table.

table1 <- table(Q2_13$start.terminal)

# Do the same thing. This time, the result is a data frame.

df1 <- aggregate(ones ~ start.terminal  ,data = Q2_13 ,FUN = sum )

# We can aggregate any numerical column in this form, and we can use any function.
# For example, here we add the durations in seconds, by start terminal.

dfjunk <- aggregate(durationsec ~ start.terminal  ,data = Q2_13 ,FUN = sum )

# Playing with this a bit more, merge the number of rides (in df1)
# into this data frame, and then compute the average ride duration
# for each start terminal.

dfjunk <- merge(df1, dfjunk, by="start.terminal")
dfjunk$mean <- dfjunk$durationsec/ dfjunk$ones

# Of course, the same thing can be done by just using the 
# mean function instead of the sum.

df2 <- aggregate(durationsec ~ start.terminal, data = Q2_13, FUN = mean)

# We can also compute the standard deviations of the durations,
# by start terminal

df2 <- aggregate(durationsec ~ start.terminal, data = Q2_13, FUN = sd)

# Computed the median duration, by start terminal and subscription type.
# All casual riders come first, then all registered riders.

df2 <- aggregate(durationsec ~ start.terminal + subscription.type, data = Q2_13, FUN = median)

# The same thing, organize differently.
# Results are ordered by terminal.

df2 <- aggregate(durationsec ~ subscription.type + start.terminal, data = Q2_13, FUN = median)

# Find the median ride duration between any two terminals, by subscription type.

df3 <- aggregate(durationsec ~ subscription.type + start.terminal + end.terminal, data = Q2_13, FUN = median)

# Turns out that there are only about 40,000 records,
# although there are about 108,000 possible combinations.
# The reason is that many combinations never appear,
# and many other combinations have very few rides in them.

# Therefore, count the number of rides between any two pairs of stations, 
# by subscription type

df4 <- aggregate(ones ~ subscription.type * start.terminal * end.terminal, data = Q2_13, FUN = sum)

# We observed that there were about 60 rides 
# which started and ended in terminal 31002.
# Let's take a look at them.

# First extract all records which start at terminal 31002.

mylogical <- Q2_13$start.terminal == 31002

# Out of these, look at all rides which also end at this terminal.

mylogical <- mylogical & (Q2_13$end.terminal == 31002)

# We are only interested in rides by subscribers,
# So let's extract these also.

mylogical <- mylogical & (Q2_13$subscription.type == "Subscriber")

# Make a data frame of only those rides:

df5 <- Q2_13[mylogical,]

# There many rides which last only a few seconds.
# Plucked the empirical cumulative distribution function
# of this subset of rides.

# There are several horizontal stretches in the graph.
# These are intervals for which no ride duration was observed.
# Therefore, these form natural "class boundaries".

plot.ecdf(df5$durationsec)

grid(col = 2)

# We can also plug the empirical cdf for all ride durations.

plot.ecdf(Q2_13$durationsec)

grid(col = 2)

# It turns out that this plot does not tell us very much,
# mainly since there are a few rides which last a long time
# so the plot window is distorted.

# Possible solution: make an empirical cdf plot of the
# logarithms (base 10) of durations.
# Add 1 second to all ride durations to avoid problems with log(0).

# The result is a fairly nice curve.

x = log10(Q2_13$durationsec+1)
plot.ecdf(x)
grid(col = 2)

# Some other calculations. Find a fraction of all casual riders.
# The command makes a logical vector which is interpreted as a 01 vector.
# The mean() computes the fraction of 1's.

mean(Q2_13$subscription.type == "Casual")

# Same thing, find the fraction of all rides that last less than 2 min.

mean(Q2_13$durationsec < 120)


df5 <- aggregate(ones ~ start.terminal * end.terminal, data = Q2_13, FUN = sum)

table3 <- table(df5$ones)
