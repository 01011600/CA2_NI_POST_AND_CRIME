# combine all of the crime data from each csv file into one dataset called AllNICrimeData.
crime_data <- list.files('Crime/', recursive = TRUE, full.names = TRUE)
AllNICrimeData <- do.call(rbind, lapply(crime_data, read.csv))

# Write the Crime data files to CSV file called AllNICrimeData
write.csv(AllNICrimeData, file = "AllNICrimeData.csv")
# 
nrow(AllNICrimeData)

# Remove the columns Crime.ID, Reported.by, Falls.within, LSOA.code, LSOA.name,Last.outcome.category and Context
Delete_Cols <- c("Crime.ID","Reported.by","Falls.within","LSOA.code","LSOA.name","Last.outcome.category","Context")
#Delete all the columns that are in the vector called Delete_Cols
AllNICrimeData <- AllNICrimeData[,!(names(AllNICrimeData) %in% Delete_Cols)]

# Check to see what class crime.type is
class(AllNICrimeData$Crime.type)
# Assign factor type to crime.type
AllNICrimeData$Crime.type <- as.factor(AllNICrimeData$Crime.type)
# Check to see what class crime.type is post the change
class(AllNICrimeData$Crime.type)

head(AllNICrimeData, 5)
# Using the gsub function to remove the "On or near" from the Location column
AllNICrimeData$Location <- gsub("On or near ", "", AllNICrimeData$Location)
#After removing the "On or near", replace empty cells with NA in the location column
AllNICrimeData[AllNICrimeData$Location == "", c("Location")] <- NA 
str(AllNICrimeData)

# Creating a dataframe called AllNICrimeData_noNA which has the NA's removed
AllNICrimeData_noNA <- AllNICrimeData[which(!is.na(AllNICrimeData$Location)),]
# Choose a random sample of 1000 entries from the AllNICrimeData_noNA dataframe
random_crime_sample<-AllNICrimeData_noNA[c(as.integer(runif(1000,1,nrow(AllNICrimeData_noNA)))),]

find_a_postcode <- function(without_postcode){
library(doBy)
CleanNIPostcodeData<-read.csv("CleanNIPostcodeData.csv")[c("Primary.Thorfare","Postcode")]
CleanNIPostcodeData$test<-paste0(CleanNIPostcodeData$Primary.Thorfare," | ",CleanNIPostcodeData$Postcode)

# Summarize (Count number of rows (Postcodes) groupped by Primary.Thorfare)
DF<-summaryBy(data = CleanNIPostcodeData, Primary.Thorfare ~ Primary.Thorfare+Postcode,FUN=length)
# getting rid of rows with NAs
DF<-na.omit(DF)

# NEW_DF<-DF[1,]
#Getting the maximum value for each location giving us the most popular postcode

NEW_DF<-summaryBy(Primary.Thorfare.length ~ Primary.Thorfare, data = DF,FUN = max)
names(NEW_DF)[2]<-"Primary.Thorfare.length"

NEW_NEW_DF<-merge(NEW_DF,DF,all.x = TRUE,all.y = FALSE)




#NEW_DFDF<-merge(DF,NEW_DF,all.x=FALSE, all.y = TRUE, by.x=c("Primary.Thorfare","Primary.Thorfare.length"),
#                by.y = c("Primary.Thorfare","Primary.Thorfare.length.max"))



# NEW_DF<-""
# for (i in 1: length(unique(DF$Primary.Thorfare))){
#   
#   TEMP_NEW_DF<-DF[1,]
#   TEMP_NEW_DF<-""
#   
#   TEMP_NEW_DF<-arrange(DF[which(DF$Primary.Thorfare==as.character(unique(DF$Primary.Thorfare)[i])),],desc(Primary.Thorfare.length))[1,]
#   
#   NEW_DF<-rbind(NEW_DF,TEMP_NEW_DF)
#   
#   
# }




  
  
return(with_postcodes)  
}





find_a_postcode <- function(Location_Col){
  library(dplyr)
  #Import NI postcode data
  NIPostCode <- readRDS("PostCode_Data/NIPCode.rds")
  #Extract most frequent postcode for location (Speeds up later processing)
  NIPostCode <- NIPostCode %>% group_by(Primary.Thorfare) %>% summarize (Postcode =names(which.max(table(Postcode))))
  
  # If Location column is not factor then convert
  if(!is.factor(Location_Col)){
    Location_Col <- as.factor(Location_Col)
  }
  
  # Extract levels from location column
  List_of_levels <- as.list(levels(Location_Col))
  
  # Initiate counter for looping through a list
  counter <- 1
  # Loop through each item in list of locations (Factor levels from location column)
  for (i in List_of_levels){
    #Extract matching locations with postcode that are NOT NA
    Location_Match <- NIPostCode[NIPostCode$Primary.Thorfare == toupper(i) & !is.na(NIPostCode$Primary.Thorfare), c("Primary.Thorfare","Postcode")]
    # If location is matched then proceed with contents of if statement
    if(nrow(Location_Match) > 0){
      if( counter == 1 ){ #If first loop iteration create and fill the dataframe
        Locations_Postcodes <- Location_Match
      } else {
        Locations_Postcodes <- rbind(Locations_Postcodes, Location_Match)
      }
    }
    rm(Location_Match) # Removing Temp file for next iteration
    counter <- counter + 1 #Keeping count of iterations, as i is a list of level attributes
  }
  return(Locations_Postcodes)
}

Postcodes <- find_a_postcode(AllNICrimeData$Location)
str(Postcodes)
nrow(Postcodes)

#Saving matched postcodes for matched locations on NI Crime data
write.csv(Postcodes, file = "Matched_loc_postcodes.csv")

# F
AllNICrimeData$Location <- toupper(AllNICrimeData$Location)

AllNICrimeData <- merge(AllNICrimeData, Postcodes, by.x = "Location", by.y = "Primary.Thorfare", all.x = TRUE, sort = TRUE)
str(AllNICrimeData)

# G
tidy_location <- function(Data){
  Missing <- Data[is.na(Data$Location), c("Longitude","Latitude")]
  Not_Missing <- Data[!is.na(Data$Location), c("Longitude","Latitude","Location")]
  
  library(geosphere)
  library(dplyr)
  # Take the avg long and lat values for data without missing locations (to improve speed)
  Long_Lat_Avg <- Not_Missing %>% group_by(Location) %>% summarise_each(funs(mean)) %>% ungroup()
  
  start <- Sys.time()
  # creates distance matrix, distance of missing locations to locations not missing
  Distances <- distm(Missing[,c('Longitude','Latitude')], Long_Lat_Avg[,c('Longitude','Latitude')], fun=distHaversine)
  stop <- Sys.time()
  # assign the Location to the point in Missing location data based on shortest distance in the matrix
  Missing <- Missing %>% mutate(Location_new = Long_Lat_Avg$Location[max.col(-Distances)])
  Missing[!is.na(Missing$Location_new),] <- Long_Lat_Avg$Location[max.col(-Distances)]
  return(Missing)
}

Missing_Locations <- tidy_location(AllNICrimeData)
str(Missing_Locations)
nrow(Missing_Locations)

#H
NIPostCode <- readRDS("PostCode_Data/NIPCode.rds")
AllNICrimeData <- merge(AllNICrimeData, NIPostCode[,c("Primary.Thorfare","Town","County","Postcode")], 
                        by.x = "Location", by.y = "Primary.Thorfare", sort = TRUE)
AllNICrimeData$Town <- as.factor(AllNICrimeData$Town) 
AllNICrimeData$County  <- as.factor(AllNICrimeData$County)

str(AllNICrimeData)

#I
write.csv(AllNICrimeData, file = "FinalNICrimeData.csv")

#J

AllNICrimeData[AllNICrimeData$Location == 'Strabane' & AllNICrimeData$Postcode == 'BT82',][1:10]
