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
#AllNICrimeData_noNA <- AllNICrimeData[which(!is.na(AllNICrimeData$Location)),]
AllNICrimeData_noNA<-na.omit(AllNICrimeData)
# Choose a random sample of 1000 entries from the AllNICrimeData_noNA dataframe
random_crime_sample_no_PC<-AllNICrimeData_noNA[c(as.integer(runif(1000,1,nrow(AllNICrimeData_noNA)))),]

# Your input is "random_crime_sample_no_PC"
find_a_postcode <- function(without_postcode){
library(doBy)
#CleanNIPostcodeData<-read.csv("CleanNIPostcodeData.csv")[c("Primary.Thorfare","Postcode")]
CleanNIPostcodeData<-read.csv("CleanNIPostcodeData.csv")
# Summary of the postcode data grouped by Primary.Thorfare to get the most popular postcode
Postcode_Summary <- summaryBy(data = CleanNIPostcodeData, Primary.Thorfare ~ Primary.Thorfare+Postcode,FUN=length)
# Remove all the rows with NA's
Postcode_Summary<-na.omit(Postcode_Summary)

#NEW_DF<-Postcode_Summary[,1]

#Getting the maximum value for each location giving us the most popular postcode
Most_Freq_Address_by_Postcode<-summaryBy(Primary.Thorfare.length ~ Primary.Thorfare, data = Postcode_Summary,FUN = max)
Most_Freq_Address_by_Postcode$Postcode<-""

for (i in 1:nrow(Most_Freq_Address_by_Postcode)){
  temp_Primary.Thorfare<-as.character(Most_Freq_Address_by_Postcode[i,c("Primary.Thorfare")])
  temp_Primary.Thorfare.length.max<-as.character(Most_Freq_Address_by_Postcode[i,c("Primary.Thorfare.length.max")])
  temp_Postcode<-as.character(Postcode_Summary[which(Postcode_Summary$Primary.Thorfare==temp_Primary.Thorfare &
                        Postcode_Summary$Primary.Thorfare.length==temp_Primary.Thorfare.length.max),c("Postcode")])
  Most_Freq_Address_by_Postcode[i,c("Postcode")]<-temp_Postcode[1]# if there are multiple matches take row 1
  
  print(paste0((i/nrow(Most_Freq_Address_by_Postcode)*100),"%"))
  
}

return(Most_Freq_Address_by_Postcode)  
}


#Calling function and applying on "random_crime_sample_no_PC"
Most_Freq_Address_by_Postcode<-find_a_postcode(random_crime_sample_no_PC)


random_crime_sample_no_PC$Location<-toupper(random_crime_sample_no_PC$Location)

random_crime_sample <- merge(random_crime_sample_no_PC,Most_Freq_Address_by_Postcode,by.x = c("Location"),by.y = c("Primary.Thorfare"),
                             all.x = TRUE)


# retaining only columns given
updated_random_sample<-random_crime_sample[,c("Month","Longitude","Latitude",
                                            "Location","Crime.type","Postcode")]
chart_data <- updated_random_sample

library(dplyr)
#chart_data<-arrange(chart_data,Postcode) # SORTS ALL
chart_data<-arrange(chart_data[grepl("BT1",chart_data$Postcode),],Postcode) # SORTS where chart_data Post only the subset where Postcode is BT1...

chart_data_summary<-as.data.frame(summary(chart_data$Crime.type))

#barplot(chart_data_summary[,1],col='blue',xlab = "Crime type", ylab = "Crime Count",names.arg = rownames(chart_data_summary),horiz = FALSE)

a=barplot(chart_data_summary[,1], col='red' , las=1, names.arg="",main = "Crime Data Count by Location",
          ylab="Crime Count")
text(a[,1], -3.7, srt = 60, adj= 1, xpd = TRUE, labels = rownames(chart_data_summary)  , cex=1.2)

