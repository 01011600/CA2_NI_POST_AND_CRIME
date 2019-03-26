# Read in the the NI post code file and assign to variable 
NIPostcodes <- read.csv("NIPostcodes.csv")
colnames(NIPostcodes) <- c("Organisation Name", "Sub-building Name", "Building Name",
                           "Number", "Primary Thorfare", "Alt Thorfare",
                           "Secondary Thorfare", "Locality", "Townland",
                           "Town", "County", "Postcode", "x-coordinates",
                           "y-coordinates", "Primary Key") 
# using str print the structure
str(NIPostcodes)
# Print the first 10 rows of NIPostcodes
head(NIPostcodes, 10)
NIPostcodes[1:10,]
library(VIM)
missing_values <- aggr(NIPostcodes, prop = TRUE, numbers = TRUE)

summary(missing_values)