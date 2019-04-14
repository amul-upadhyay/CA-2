                         

# Importing Ni_Postcode Data into R
# The Header is kept false, because our data does not have  header fileds.

ni_postcode_df<-  read.csv("NIPostcodes.csv", header = FALSE)

# Checking class 

class(ni_postcode_df)

# Checking type of dataframe

typeof(ni_postcode_df)




                            #SECTION (1)


####################################################################################
# (A)  Show the total number of rows, the structure of the data frame, and first 10#
# rows of the data frame containing all of the NIPostcode data                     #
####################################################################################


# Total Number of Rows and Columns

nrow(ni_postcode_df)

# Viewing Structure of data

str(ni_postcode_df)

#Viewing top 10 rows of datadrame

head(ni_postcode_df,10)



###############################################################################
#                                                                             #    
#   (B)   Add a suitable title for each attribute of the data.                #
#                                                                             #
###############################################################################


# Assigning header or column_names to our data frame

names(ni_postcode_df) <- c("Organization Name", "Sub-building Name", "Building Name", "Number", "Primary THoroughfare", "Alt Thoroughfare", "Secondary Thorughtfare", "Locality", "Townland", "Town", "County", "Postcode", "X-Cordinate", "Y-Cordinate", "Primary Key")


colnames(ni_postcode_df)

###########################################################################################
#   (c) Remove or replace missing entries with a suitable identifier. Decide whether it is#
#           best to remove missing data or to recode it.                                  #
###########################################################################################


# Checking Missing Values

sum(is.na(ni_postcode_df))

#Replacing Missing Values with NA

ni_postcode_df[ni_postcode_df == ""] <- NA

sum(is.na(ni_postcode_df))

#############################################################################
#   (D) Show the total number and mean missing values for each column in the#
#       postcode data frame.                                                #
#############################################################################

# Misisng values according to columns

sapply(ni_postcode_df, function(x) sum(is.na(x)))

#Listing frequency of counties

table(ni_postcode_df$County)

#Installinf plyr package

#install.packages("plyr")
#library(plyr)


##############################################################################
#   (E) Modify the County attribute to be a categorising factor.             #
#                                                                            #
##############################################################################


# CAtegerozing county

ni_postcode_df$county_categorized <- revalue(ni_postcode_df$County, c("ANTRIM"="1", "ARMAGH"="2", "DOWN" = "3", "FERMANAGH" = "4", "LONDONDERRY" = "5", "TYRONE" = "6"))

#####################################################################
#   (f) Move the primary key identifier to the start of the dataset.#
#####################################################################

# Moving Primary key column to first Positon.

ni_postcode_df <- ni_postcode_df[, c(15, 1:14,16)]

head(ni_postcode_df,5)

####################################################################################
#   (G) Create a new dataset called Limavady_data. Store within it only information#
# that has locality, townland and town containing the name Limavady. Store this    #
# information in an external csv file called Limavady.                             #
####################################################################################
# NEw dataset

#install.packages("dplyr")

#library(dplyr)

##Limvady_data <- subset(ni_postcode_df, select = c("Locality", "Townland", "Town"))
#install.packages("stringr")
#library(stringr)

Limvady_data <- ni_postcode_df %>%
  
filter(str_detect(Town,fixed("LIMAVADY")) & str_detect(ni_postcode_df$Locality, fixed("LIMAVADY")) & str_detect(ni_postcode_df$Townland, fixed("LIMAVADY")))

head(Limvady_data, 5)

nrow(Limvady_data)

write.csv(Limvady_data, "Limavady.csv")

##################################################################
#  (H) Save the modified NIPostcode dataset in a csv file called #
#   CleanNIPostcodeData.                                         #
##################################################################

write.csv(ni_postcode_df, "CleanNIPostcodeData.csv")



