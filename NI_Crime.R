
## Setting working directory where all crime data files are present.

install.packages("RCurl")
library(RCurl)


                              #SECTION (2)


#####################################################################################
# (A) Using R, amalgamate all of the crime data from each csv file into one dataset.#
# Save this dataset into a csv file called AllNICrimeData. Count and show the number#
# of rows in the AllNICrimeData dataset.                                            #  
####################################################################################

# Downloading Zip file from GitHub and naming it NICrimeData.zip

download.file("https://github.com/amul-upadhyay/CA-2/raw/master/NI%20Crime%20Data.zip",
              destfile = "NICrimeData.zip")

# Unzipping NICrimeData.zip downloaded from GitHub

unzip("NICrimeData.zip")

# The below step will go through the files in that folder, pick out the filenames,
# And pass them into the variable ‘filenames’. 
# We’ll now use those file names to iterate through the files, 
# Extracting the data from each csv file, 
# And combining it into one list.

filenames <- list.files("NI Crime Data/", full.names = TRUE, recursive = TRUE)

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

colnames(AllNICrimeData) <- c("Crime ID", "Month", "Reported", "Falls within",
                              "Longitude", "Latitude", "Location", "LSOA code",
                              "LSOA name", "Crime type", "Last outcome category", "Context")

nrow(AllNICrimeData)

write.csv(AllNICrimeData, file = "AllNICrimeData.csv" )



#################################################
#                   (B)                         #
#################################################

# Column names are checked.

colnames(AllNICrimeData)

# Appropriate column names are asssigned to specific columns.

AllNICrimeData <- AllNICrimeData[, !(names(AllNICrimeData) %in% 
                                c("Crime ID", "Reported", "Falls within", "LSOA code", "LSOA name",
                                  "Last outcome category", "Context"))]

AllNICrimeData

str(AllNICrimeData)


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

#Removing Stop words, for example 'ALL or near' from all the values of Location column.

AllNICrimeData$Location <- gsub("On or near", "\\1", AllNICrimeData$Location)

# There may be some fields which only had On or near, hence Assigning NA to blank cells.

AllNICrimeData[AllNICrimeData == " "] <- NA

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

# White spaces are trimmed from the dataframe.

AllNICrimeData$Location <- str_trim(AllNICrimeData$Location, "both")

AllNICrimeData$Location <- trimws(AllNICrimeData$Location)

#All blank values, if present were filled with NA's.

AllNICrimeData[AllNICrimeData == ""] <- NA

# Location Column is filtred not to include NA's.

AllNICrimeData <- AllNICrimeData[!is.na(AllNICrimeData$Location),]

random_crime_sample <- AllNICrimeData[sample(nrow(AllNICrimeData), 1000), ]

random_crime_sample

nrow(random_crime_sample)


#################################################
#               CLean    NIPOSTCODEDATA         #
#################################################

# The Header is kept false, because our data does not have a header fileds.

Clean_NI_Postocde_CSV <- getURL("https://media.githubusercontent.com/media/amul-upadhyay/CA-2/master/CleanNIPostcodeData.csv")

#  Converting to data frame

Clean_NI_Postocde_df <- read.csv(text = Clean_NI_Postocde_CSV)

# Viewing head of the data frame

head(Clean_NI_Postocde_df)

# Trimming Postcode

Clean_NI_Postocde_df$Postcode <- trimws(Clean_NI_Postocde_df$Postcode)

# Trimming PRimary Throughfare 

Clean_NI_Postocde_df$Primary.THoroughfare <- trimws(Clean_NI_Postocde_df$Primary.THoroughfare)

# Trimming Random Crime SAmple

random_crime_sample$Location <- trimws(random_crime_sample$Location)

# Converting Cases to upper

random_crime_sample$Location <- toupper(random_crime_sample$Location)


# Writing a function that will extract postcoce from clean_postcocde_data and 
# will return the postcode that appears most frequently for a 
# specific location.

random_crime_sample$Postcode<- apply(as.matrix(random_crime_sample$Location), MARGIN=1, function(x)(
  {
    
    Filtered_Data <- Clean_NI_Postocde_df %>% filter(Primary.THoroughfare == x)
    
    Filtered_PostCode_Data <- Filtered_Data$Postcode
    
    Filtered_PostCode_Data <- na.exclude(Filtered_PostCode_Data)
    
    names(which.max(table(Filtered_PostCode_Data)))
    
  }))


# Converting Postocde that is in list format to factor

random_crime_sample$Postcode <- vapply(random_crime_sample$Postcode, paste, collapse = ", ", character(1L))

# Viewing head of random_crime_sample

head(random_crime_sample)

class(random_crime_sample)

write.csv(random_crime_sample,file = "random_crime_sample.csv")


######################################################
#                   G                                #
######################################################

#install.packages('gtools')

#library(gtools)

# Creating Updated Random sample

updated_random_sample <- random_crime_sample[,-6, drop = FALSE]

# Viewing head of updated_random_sample

head(updated_random_sample, 5)

# REmoving White Spaces

updated_random_sample$Postcode <- trimws(updated_random_sample$Postcode)

# Sort the chart_data data frame by postcode where the postcode contains "BT1"

chart_data <- updated_random_sample

chart_data$Postcode <-  mixedsort(chart_data$Postcode)

head(chart_data)

# Sort the chart_data data frame by crime.type

chart_data$Crime.type <- sort(chart_data$Crime.type)

head(chart_data)


###############################################
#                (H)                          #
###############################################

# here we gets the colors for unique crime type
palette <- RColorBrewer::brewer.pal(length(unique(chart_data$Crime.type)),name = 'Set1')

# Lets create a frequency table for types of Crime.
counts <- table(chart_data$Crime.type)
print(counts)

# Create a bar plot of the crime type from the chart_data data frame. 
barplot(counts,
        xlab='Offence Type',ylab='Crime Rate',
        main='Crime Statistics',
        col=counts,col.main='Blue')















