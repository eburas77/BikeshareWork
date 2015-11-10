# Change all names to lower case

names(Q2_13) <- tolower(names(Q2_13))

# We wish to make a data frame that tells us the terminal number number (start.terminal)
# for each start station (start.station).
# This can be done with the "aggregate" operation.
# Using the data frame specified in the second argument (data =),
# apply the function that is specified in the third argument (FUN =)
# to the data frame column that appears on the left of ~ (in this case, start.terminal),
# grouped by the data frame column(s) to the right of ~ (In this case, start.station)
# 
# That is, for each start station, find all terminal numbers that appear
# and write them in a new data frame.

# The aggregate command is extremely useful, become comfortable using it!

startstations <- aggregate(start.terminal ~ start.station , data = Q2_13, FUN = unique)

# We can do the same thing with end stations.

endstations <- aggregate(end.terminal ~ end.station , data = Q2_13, FUN = unique)

# Combine the two data frames by merging them, using the terminal number as merge variable.

allstations = merge(startstations, endstations, by.x = "start.terminal", by.y = "end.terminal")

# The station names are still levels of factors at this point, so we change them to  character

allstations$start.station <- as.character(allstations$start.station)
allstations$end.station <- as.character(allstations$end.station)

# Determine where start stations and end stations differ, although terminal numbers are the same:
# This was not done in our session on Tuesday.

allstations$different = (allstations$start.station != allstations$end.station)

# How many such cases are there?

sum(allstations$different)

# The answer is 12. Let's look at these in more detail:

# Make a table of all start terminal numbers and determine how often they appear.
# Sort by decreasing frequency and look at the top of the table.
# There are six terminal numbers which appear twice. 
# Extract these terminal numbers and call them  "duplicates".

starttable <- table(startstations$start.terminal)
sort(decreasing =TRUE, starttable) -> starttable
as.numeric(names(head(starttable,6))) -> duplicates

# We then ordered all start stations by increasing terminal number
# and looked at the case where there are duplicates,
# in order to find out why these duplicates occurred.

myorder <- order(startstations$start.terminal)
startstations <- startstations[myorder,]

# There were three different reasons:
# The start station had been moved ("formerly...")
# The street address was written differently (18th St becomes 18th)
# An address had been incorrect and was fixed (Randle Pl NE became Randle PL SE)

# We decide what to do in each case and delete the corresponding rows in the data frame 
# startstations

startstations[-c(78, 96, 113, 163, 189, 224),] -> startstations 

# Make a new data frame that is Q2_13 without the start station:

Q2new <- Q2_13
Q2new$start.station <- NULL

# The start station name has been deleted.
# We can put it back in by merging with the data frame startstations
# which contains each start station name next to the terminal number.

Q2new <- merge(Q2_13, startstations, by = "start.terminal")




