
# Setting working directory where all crime data files are present.

setwd("~/CA-2/NI Crime Data")

# The below step will go through the files in that folder, pick out the filenames,
# And pass them into the variable ‘filenames’. 
# We’ll now use those file names to iterate through the files, 
# Extracting the data from each csv file, 
# And combining it into one list.

filenames <- list.files(full.names = TRUE, recursive = TRUE)

# Vieawing contents of the list

filenames

# This pushes the data into the variable 'All_Crime_Data'. 

All_Crime_Data <- lapply(filenames,function(i){
  read.csv(i, header=FALSE, skip=4)
})

All

# To use the datasets, either to do matching in R,
# Or to save as its own csv file, 
# we need to turn it into a dataframe.
# To do this, we can run the following:

AllNICrimeData <- do.call(rbind.data.frame, All)

head(AllNICrimeData)

colnames(AllNICrimeData) <- c("Crime ID", "Month", "Reported", "Falls within", "Longitude", "Latitude", "Location", "LSOA code", "LSOA name", "Crime type", "Last outcome category", "Context")

nrow(AllNICrimeData)

write.csv(AllNICrimeData, file = "AllNICrimeData.csv" )



