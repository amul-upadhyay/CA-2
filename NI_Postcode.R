
# Importing Ni_Postcode Data into R
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


# Checking Missing Values

sum(is.empty.model(ni_postcode_df))

#Replacing Missing Values with NA

ni_postcode_df[ni_postcode_df == ""] <- NA



sum(is.na(ni_postcode_df))


# Misisng values according to columns

sapply(ni_postcode_df, function(x) sum(is.na(x)))

#Listing frequency of counties

table(ni_postcode_df$County)

#Installinf plyr package

#install.packages("plyr")
#library(plyr)

# CAtegerozing county

ni_postcode_df$county_categorized <- revalue(ni_postcode_df$County, c("ANTRIM"="1", "ARMAGH"="2", "DOWN" = "3", "FERMANAGH" = "4", "LONDONDERRY" = "5", "TYRONE" = "6"))

# Moving Primary key column to first Positon

ni_postcode_df <- ni_postcode_df[, c(15, 1:14,16)]

head(ni_postcode_df,5)

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

write.csv(ni_postcode_df, "CleanNIPostcodeData.csv")



