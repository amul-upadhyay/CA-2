
## Setting working directory where all crime data files are present.

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

All_Crime_Data

# To use the datasets, either to do matching in R,
# Or to save as its own csv file, 
# we need to turn it into a dataframe.
# To do this, we can run the following:

AllNICrimeData <- do.call(rbind.data.frame, All_Crime_Data)

head(AllNICrimeData)

colnames(AllNICrimeData) <- c("Crime ID", "Month", "Reported", "Falls within", "Longitude", "Latitude", "Location", "LSOA code", "LSOA name", "Crime type", "Last outcome category", "Context")

nrow(AllNICrimeData)

setwd("~/CA-2")

write.csv(AllNICrimeData, file = "AllNICrimeData.csv" )



#################################################
#                   (B)                         #
#################################################


typeof(AllNICrimeData)


colnames(AllNICrimeData)

asd <- AllNICrimeData[, !(names(AllNICrimeData) %in% c("Crime ID", "Reported", "Falls within", "LSOA code", "LSOA name", "Last outcome category", "Context"))]
asd


str(asd)


#################################################
#                   (C)                         #
#################################################

table(AllNICrimeData$`Crime type`)

install.packages("plyr")
library(plyr)


AllNICrimeData$'crime type categorized' <- revalue(
AllNICrimeData$`Crime type`, c("Anti-social behaviour"="Summary Offences", 
                              "Bicycle theft"="Property Crimes",
                              "Burglary" = "Property Crimes",
                              "Criminal damage and arson" = "Property Crimes", 
                              "Drugs" = "Statutory Crimes",
                              "Other crime" = "Other", "Other theft" = "Other",
                              "Possession of weapons" = "Incholate Crime",
                              "Public order" = "summary offences", "Robbery" = "Property Crimes",
                              "Shoplifting" = "Property Crimes", 
                              "Theft from the person" = "Personal Crimes",
                              "Vehicle crime" = "Statutory Crimes",
                              "Violence and sexual offences" = "Personal Crimes"))


head(AllNICrimeData) 



#################################################
#                   (D)                         #
#################################################

install.packages("tm")
library(tm)




stopwords("en")
X <- removeWords(stopwords, AllNICrimeData$Location)


AllNICrimeData$Location <- gsub("On or near", "\\1", AllNICrimeData$Location)
head(AllNICrimeData)




