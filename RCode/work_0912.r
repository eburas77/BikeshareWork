# Explore POSICct, POSIXlt etc to fnd out about weekdays extraction

# Convert character string as it appears in the bikeshare files to POSIXlt format:

mydate = strptime("9/12/2013",format = "%m/%d/%Y")

# Extract weekday:
  
weekdays(mydate)  

# Extract month:

months(mydate)

# Extract quarter:

quarters(mydate)

# Function to separate time stamp found in csv files into character strings for dates and time:

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