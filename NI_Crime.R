
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


str(AllNICrimeData)


######################################################################################
#   (B)  Modify the structure of the newly created AllNICrimeData csv file and remove#
# the following attributes: CrimeID, Reported by, Falls within, LSOA code, LSOA name,#
# last outcome and context. Show the structure of the modified file.                 #      
######################################################################################

# Column names are checked.

colnames(AllNICrimeData)

# Appropriate column names are asssigned to specific columns.

AllNICrimeData <- AllNICrimeData[, !(names(AllNICrimeData) %in% 
                                c("Crime ID", "Reported", "Falls within", "LSOA code", "LSOA name",
                                  "Last outcome category", "Context"))]

AllNICrimeData

str(AllNICrimeData)


#########################################################################
#  (C)  Factorise the Crime type attribute. Show the modified structure.#                        #
#########################################################################

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



###########################################################################################
#   (D)   Modify the AllNICrimeData dataset so that the Location attribute contains only a#
# street name. For example, the attribute value “On or near Westrock Square” should       #
# be modified to only contain “Westrock Square”. Modify the resultant empty location      #
# attributes with a suitable identifier.                                                  #
###########################################################################################

#install.packages("tm")
#library(tm)

#Removing Stop words, for example 'ALL or near' from all the values of Location column.

AllNICrimeData$Location <- gsub("On or near", "\\1", AllNICrimeData$Location)

# There may be some fields which only had On or near, hence Assigning NA to blank cells.

AllNICrimeData[AllNICrimeData == " "] <- NA

head(AllNICrimeData)




#######################################################################################
# (E)   Choose 1000 random samples of crime data from the AllNICrimeData datase       #
# where the location attribute contains location information. This means that the     #
# location information should NOT contain an NA identifier. Store this data in a data #
# frame called random_crime_sample. Then create a function called find_a_postcode     #
# that takes as an input each location attribute from random_crime_sample and finds   #
# a suitable postcode value from the postcode dataset.                                #
#                                                                                     #
# Use the CleanNIPostcodeData dataset you created in section 1 as the reference       #
# data to find postcodes. If there are several postcodes discovered with the same     #
# location, choose the most popular postcode for that location. Store the output from #
# the find_a_postcode function in a suitably named variable in your random_crime      #
# sample data frame. Make sure there are no missing postcodes in the output from      #
# your function. Show the structure of this data frame once you’ve completed this task#
# and count the number of records in the modified random_crime_sample data            #
# frame.                                                                              #
#######################################################################################

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

random_crime_sample <-unique(sample_n(AllNICrimeData[complete.cases(AllNICrimeData),], 1000))

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

Clean_NI_Postocde_df$Primary.THoroughfare <- str_squish(Clean_NI_Postocde_df$Primary.THoroughfare)


# Trimming Random Crime SAmple

random_crime_sample$Location <- trimws(random_crime_sample$Location)

random_crime_sample$Location <- str_squish(random_crime_sample$Location)


# Converting Cases to upper

random_crime_sample$Location <- toupper(random_crime_sample$Location)


# Writing a function that will extract postcoce from clean_postcocde_data and 
# will return the postcode that appears most frequently for a 
# specific location.

random_crime_sample$Postcode<- apply(as.matrix(random_crime_sample$Location), MARGIN=1, function(x)(
  {
    
    Filtered_Data <- Clean_NI_Postocde_df %>% filter(Primary.THoroughfare == x)
    
    Filtered_PostCode_Data <- Filtered_Data$Postcode
    
    random_crime_sample$Location <- as.character(random_crime_sample$Location)
    
    names(which.max(table(Filtered_PostCode_Data)))
    
  }))

random_crime_sample[random_crime_sample == ""] <- NA

random_crime_sample <- random_crime_sample[!is.na(random_crime_sample$Postcode), ]

# Converting Postocde that is in list format to factor

random_crime_sample$Postcode <- vapply(random_crime_sample$Postcode, paste, collapse = ", ", character(1L))

# Viewing head of random_crime_sample

head(random_crime_sample)

nrow(random_crime_sample)

class(random_crime_sample)

write.csv(random_crime_sample,file = "random_crime_sample.csv")

colnames(random_crime_sample)


#########################################################################################
#   (G) ) Create a bar plot of the crime type from the chart_data data frame. Show a    #
# suitable main title for the bar chart, and suitable x and y-axis labels. Make sure all#
# labels on the x-axis can be read. Show the bar plot in your CA document.              #                #
#########################################################################################

# install.packages('gtools')

 #library(gtools)

# Creating Updated Random sample

updated_random_sample <- random_crime_sample[,-6, drop = FALSE]

# Viewing head of updated_random_sample

head(updated_random_sample, 5)

# REmoving White Spaces

updated_random_sample$Postcode <- trimws(updated_random_sample$Postcode)

# Sort the chart_data data frame by postcode where the postcode contains "BT1"

chart_data <- updated_random_sample

chart_data$Postcode <-  mixedsort(updated_random_sample$Postcode)

head(chart_data)

# Sort the chart_data data frame by crime.type

chart_data$`Crime type` <- sort(chart_data$`Crime type`)

head(chart_data)


###############################################
#                (H)                          #
###############################################



# Lets create a Bar Plot graph for types of Crime.

counts <- table(chart_data$`Crime type`)
x <- barplot(counts, xaxt="n", xlab= " Crime Type", ylab = "Number of Crimes",
             main = "Frequency of various crime types", density = 50, 
             col = "blue", border = "black")

labs <- paste(names(counts), "")

text(cex=0.85, x=x-.15, y=-0.15, labs, xpd=TRUE, srt=30, las=2)