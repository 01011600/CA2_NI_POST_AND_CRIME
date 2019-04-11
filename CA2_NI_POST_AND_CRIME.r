# Read in the NI post code file and assign to variable NIPostcodes 
NIPostcodes <-read.csv("NIPostcodes.csv",header = T,sep=",")
# using str print the structure
str(NIPostcodes)
# Print the first 10 rows of NIPostcodes
head(NIPostcodes, 10)
colnames(NIPostcodes) <- c("Organisation Name", "Sub-building Name", "Building Name",
                           "Number", "Primary Thorfare", "Alt Thorfare",
                           "Secondary Thorfare", "Locality", "Townland",
                           "Town", "County", "Postcode", "x-coordinates",
                           "y-coordinates", "Primary Key") 
str(NIPostcodes)

# replace all the blank cell vaules with NA
NIPostcodes[NIPostcodes == ""] <- NA

# count the number of missing values by column
colSums(is.na(NIPostcodes))

# Calculate the mean number of missing values for each column
colMeans(is.na(NIPostcodes))
str(NIPostcodes)

# Get the class of each Column
sapply(NIPostcodes,class)

#Change the class of County to be a categorising factor
NIPostcodes$County <- as.factor(NIPostcodes$County)

# Check to see what class County is post the change
class(NIPostcodes$County)

# Print the column names of the dataframe NIPostcodes
colnames(NIPostcodes)
# Change the order of the columns, move Primary Key to the first column
NIPostcodes <- NIPostcodes[,c("Primary Key","Organisation Name", "Sub-building Name", "Building Name",
                              "Number", "Primary Thorfare", "Alt Thorfare",
                              "Secondary Thorfare", "Locality", "Townland",
                              "Town", "County", "Postcode", "x-coordinates",
                              "y-coordinates")]

# Print the column names of the dataframe NIPostcodes
colnames(NIPostcodes)

# Create a new dataframe called Limavady_data that contains records that have 
# Limavady in the column Locality,Townland and Town
Limavady_data<-NIPostcodes[grepl("LIMAVADY",NIPostcodes$Locality) & 
                           grepl("LIMAVADY",NIPostcodes$Townland) & 
                           grepl("LIMAVADY",NIPostcodes$Town),]

# Write Limavady data to CSV file
write.csv(Limavady_data, file = "Limavady_data.csv")

# rename the NIPostcodes dataframe as CleanNIPostcodeData
CleanNIPostcodeData <- NIPostcodes

# Write the NIPostcodes data to CSV file called CleanNIPostcodeData
write.csv(NIPostcodes, file = "CleanNIPostcodeData.csv")
