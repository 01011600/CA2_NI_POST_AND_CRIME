# Read in the the NI post code file and assign to variable 
#NIPostcodes <- read.csv("NIPostcodes.csv")
NIPostcodes <-read.csv("NIPostcodes.csv",header = T,na.strings = '',sep=",")
colnames(NIPostcodes) <- c("Organisation Name", "Sub-building Name", "Building Name",
                           "Number", "Primary Thorfare", "Alt Thorfare",
                           "Secondary Thorfare", "Locality", "Townland",
                           "Town", "County", "Postcode", "x-coordinates",
                           "y-coordinates", "Primary Key") 
# using str print the structure
str(NIPostcodes)
# Print the first 10 rows of NIPostcodes
head(NIPostcodes, 10)
#NIPostcodes[is.na(NIPostcodes),]
# Number of missing values
sum(is.na(NIPostcodes))
# Mean missing values
mean(is.na(NIPostcodes))
