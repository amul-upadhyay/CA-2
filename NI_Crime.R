
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

#install.packages("plyr")
#library(plyr)


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

#install.packages("tm")
#library(tm)


AllNICrimeData$Location <- gsub("On or near", "\\1", AllNICrimeData$Location)
head(AllNICrimeData)




#################################################
#                   (E)                         #
#################################################
#install.packages("stringr")
#install.packages("dplyr")
#install.packages("raster")

#library(stringr)
#library(dplyr)
#library(raster)

AllNICrimeData

AllNICrimeData$Location <- str_trim(AllNICrimeData$Location, "both")

AllNICrimeData$Location <- trimws(AllNICrimeData$Location)

AllNICrimeData[AllNICrimeData == ""] <- NA

AllNICrimeData <- AllNICrimeData[!is.na(AllNICrimeData$Location),]

random_crime_sample <- AllNICrimeData[sample(nrow(AllNICrimeData), 1000), ]

random_crime_sample


nrow(random_crime_sample)


#################################################
#                   NIPOSTCODEDATA              #
#################################################

# The Header is kept false, because our data does not have a header fileds.

ni_postcode_df<-  read.csv("NIPostcodes.csv", header = FALSE)

# Checking class 

class(ni_postcode_df)

# Checking type of dataframe

typeof(ni_postcode_df)


# Assigning header or column_names to our data frame

names(ni_postcode_df) <- c("Organization Name", "Sub-building Name", "Building Name", "Number", "Primary THoroughfare", "Alt Thoroughfare", "Secondary Thorughtfare", "Locality", "Townland", "Town", "County", "Postcode", "X-Cordinate", "Y-Cordinate", "Primary Key")


# Total Number of Rows and Columns

nrow(ni_postcode_df)

ncol(ni_postcode_df)

# Viewing Structure of data

str(ni_postcode_df)

#Viewing top 10 rows of datadrame

head(ni_postcode_df,10)

colnames(ni_postcode_df)

class(ni_postcode_df)
# Checking Missing Values

sum(is.empty.model(ni_postcode_df))

#Replacing Missing Values with NA

ni_postcode_df[ni_postcode_df == ""] <- NA

head(ni_postcode_df)

names(which.max(table(ni_postcode_df$Postcode)))


####################################
              (F)
####################################


ni_postcode_df$Postcode <- trimws(ni_postcode_df$Postcode)

ni_postcode_df$`Primary THoroughfare` <- trimws(ni_postcode_df$`Primary THoroughfare`)

random_crime_sample$Location <- trimws(random_crime_sample$Location)


random_crime_sample$Location <- toupper(random_crime_sample$Location)



random_crime_sample$Postcode<- apply(as.matrix(random_crime_sample$Location), MARGIN=1, function(x)(
  {
    
    Filtered_Data <- ni_postcode_df %>% filter(`Primary THoroughfare` == x)
    
    Filtered_PostCode_Data <- Filtered_Data$Postcode
    
    Filtered_PostCode_Data <- na.exclude(Filtered_PostCode_Data)
    
    names(which.max(table(Filtered_PostCode_Data)))
    
  }))

head(random_crime_sample)


nrow(random_crime_sample)










