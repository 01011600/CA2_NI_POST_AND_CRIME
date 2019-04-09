# Read in the NI post code file and assign to variable NIPostcodes 
NIPostcodes <-read.csv("NIPostcodes.csv",header = T,na.strings = '',sep=",")
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
head(NIPostcodes, 3)

#NIPostcodes[is.na(NIPostcodes),]
# Number of missing values

# Show the total number and mean missing values of the NIPostcode data
# First replace all missing data with NA
nipostcodes[NIPostcodes == ""] <- NA
# Show the total number of missing values for each attribute
colSums(!is.na(NIPostcodes))
# Show the mean number of missing values for each attribute
colMeans(is.na(NIPostcodes))

sapply(NIPostcodes,class)
class(NIPostcodes$County)
NIPostcodes$County <- as.factor(NIPostcodes$County)
class(NIPostcodes$County)
str(NIPostcodes)
colnames(NIPostcodes)
NIPostcodes <- NIPostcodes[,c("Primary Key","Organisation Name", "Sub-building Name", "Building Name",
                              "Number", "Primary Thorfare", "Alt Thorfare",
                              "Secondary Thorfare", "Locality", "Townland",
                              "Town", "County", "Postcode", "x-coordinates",
                              "y-coordinates")]
colnames(NIPostcodes)
str(NIPostcodes)

Limavady_data<-NIPostcodes[grepl("LIMAVADY",NIPostcodes$Locality) & 
                           grepl("LIMAVADY",NIPostcodes$Townland) & 
                           grepl("LIMAVADY",NIPostcodes$Town),]

Limavady_data2<-NIPostcodes[grepl("LIMAVADY",NIPostcodes$Locality) | 
                             grepl("LIMAVADY",NIPostcodes$Townland) | 
                             grepl("LIMAVADY",NIPostcodes$Town),]
