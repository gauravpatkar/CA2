
#here we are importing data from csv
df_NIPostcodes=read.csv("NIPostcodes.csv", header = 0)

#here we are showing total no. of row 
nrow(df_NIPostcodes)

#here we are showing first 10 row result of NIPostcodes
head(df_NIPostcodes,n=10)

#here we are showing structure of dataframe
str(df_NIPostcodes)


#here we are Adding suitable titles to the attributes as given in to us in report.
colnames(df_NIPostcodes) <- c("Organization Name", "Sub-building Name", "Building Name", "Number", 
                              "Primary Thorfare", "Alt Thorfare", "Secondary Thorfare", "Locality", 
                              "Townland", "Town", "County","Postcode", "x-coordinates", "y-coordinates",
                              "Primary Key")

str(df_NIPostcodes)
#here we are replacing the null value with NA

df_NIPostcodes[df_NIPostcodes==""] <- NA
df_NIPostcodes
str(df_NIPostcodes)


# here we are Showing the totat number of missing values and their mean in the dataset

colSums(is.na(df_NIPostcodes))
colMeans(is.na(df_NIPostcodes))

head(df_NIPostcodes,n=5)
str(df_NIPostcodes)




#  here we are Modifying the County attribute to be a categorising factor 
#adding a new column as "county_number" as 1,2,3,4,5,6


df_NIPostcodes$county_number[df_NIPostcodes$County == "ANTRIM"] <- 1
df_NIPostcodes$county_number[df_NIPostcodes$County ==  "LONDONDERRY"] <- 2
df_NIPostcodes$county_number[df_NIPostcodes$County == "DOWN"] <- 3
df_NIPostcodes$county_number[df_NIPostcodes$County == "ARMAGH"] <- 4
df_NIPostcodes$county_number[df_NIPostcodes$County == "TYRONE"] <- 5
df_NIPostcodes$county_number[df_NIPostcodes$County == "FERMANAGH"] <- 6


county_number <- factor(df_NIPostcodes$county_number, order = TRUE, levels = c(1,2,3,4,5,6))
df_NIPostcodes$county_number <- county_number
head(df_NIPostcodes)
str(df_NIPostcodes)



#here we are moving primary key identifier to first column 

df_NIPostcodes <- df_NIPostcodes[, c(15, 1:14, 16)]
head(df_NIPostcodes)

str(df_NIPostcodes)


#
write.csv(df_NIPostcodes, file = "CleanNIPostcodeData.csv")
head(df_NIPostcodes)
attach(df_NIPostcodes)
df_NIPostcodes_new <- df_NIPostcodes[ which(Town == "LIMAVADY"),]
Limavady_data <- df_NIPostcodes_new[c("Locality","Townland","Town")]
detach(df_NIPostcodes_new)
Limavady_data
write.csv(Limavady_data, file = "Limavady_data.csv")
str(Limavady_data)


#here we are saving NIPostcodes into new file called cleanNIPostcodesData.csv
write.csv(df_NIPostcodes_new, file = "CleanNIPostcodesData.csv", row.names = FALSE, col.names = FALSE)
str(CleanNIPostcodesData)




#section 2 
#1
#here we are adding all the files into one single csv file 
setwd("NI Crime Data")
csv_files <- dir(pattern='*.csv$', recursive = TRUE)

library(dplyr)


for(i in 1:length(csv_files)) {
  if(i == 1)
    AllNICrimeData_dataframe <- read.csv(csv_files[i])
  else
    AllNICrimeData_dataframe <- rbind(AllNICrimeData_dataframe, read.csv(csv_files[i]))
}

# here we are reading and writing the file in csv file
AllNICrimeData_dataframe <- rbind_all(lapply(csv_files, read.csv))
head(AllNICrimeData_dataframe)
setwd("..")
write.csv(AllNICrimeData_dataframe, file = "AllNICrimeData.csv")

#here we are modifying the data by altering attributes 
ncol(AllNICrimeData_dataframe)
nrow(AllNICrimeData_dataframe)

str(AllNICrimeData_dataframe)


# 2nd
#here we are creating a subset that only includes important columns
AllNICrimeData_dataframe <- subset(AllNICrimeData_dataframe, select = -c(Crime.ID, Reported.by, Falls.within, LSOA.code, LSOA.name, Last.outcome.category, Context))
str(AllNICrimeData_dataframe)

# 3rd
#AllNICrimeData_dataframe$Casualty.type = as.factor(AllNICrimeData_dataframe$casualty.type)
#here we are categorizing the crime type by using casualty type as other attribute

attach(AllNICrimeData_dataframe)
AllNICrimeData_dataframe$casualty.type[Crime.type == "Anti-social behaviour"] <- "violent crime" 
AllNICrimeData_dataframe$casualty.type[Crime.type == "Drugs"]<- "violent crime"
AllNICrimeData_dataframe$casualty.type[Crime.type == "Possession of weapons"] <- "violent crime"
AllNICrimeData_dataframe$casualty.type[Crime.type == "Public order"]  <- "violent crime"
AllNICrimeData_dataframe$casualty.type[Crime.type == "Theft from the person" ] <- "violent crime"
AllNICrimeData_dataframe$casualty.type[Crime.type == "Violence and sexual offences"] <- "violent crime"
AllNICrimeData_dataframe$casualty.type[Crime.type == "Bicycle theft"] <- "Property Damage"
AllNICrimeData_dataframe$casualty.type[Crime.type == "Burglary"] <- "Property Damage"
AllNICrimeData_dataframe$casualty.type[Crime.type == "Criminal damage and arson"] <- "Property Damage"
AllNICrimeData_dataframe$casualty.type[Crime.type == "Other crime"] <- "Property Damage"
AllNICrimeData_dataframe$casualty.type[Crime.type == "Other theft"] <- "Property Damage"
AllNICrimeData_dataframe$casualty.type[Crime.type == "Robbery"] <- "Property Damage"
AllNICrimeData_dataframe$casualty.type[Crime.type == "Shoplifting"] <- "Property Damage"
AllNICrimeData_dataframe$casualty.type[Crime.type == "Vehicle crime"] <- "Property Damage"
detach(AllNICrimeData_dataframe)
casualty.type <- factor(AllNICrimeData_dataframe$casualty.type, order = TRUE, levels = c("violent crime", "Property Damage"))


#here we are creating a new attribute for crime 
AllNICrimeData_dataframe$casualty.type <- casualty.type
head(AllNICrimeData_dataframe)

str(AllNICrimeData_dataframe)

# 4th
#Modifying the dataset such that the location attribute contains just the street name
AllNICrimeData_dataframe$Location <- gsub("On or near", '', AllNICrimeData_dataframe$Location, ignore.case = FALSE)
AllNICrimeData_dataframe$Location <- trimws(AllNICrimeData_dataframe$Location, which = "both")
AllNICrimeData_dataframe$Location <- sub("^$", "No Location", AllNICrimeData_dataframe$Location)
head(AllNICrimeData_dataframe)

str(AllNICrimeData_dataframe)

#datafile_summary <- group_by(AllNICrimeData_dataframe, Location)

#colSums(is.na(AllNICrimeData_dataframe["Location"]))
#random_crime_samples <- sample_n(AllNICrimeData_dataframe, 1000)
#sum([])

sum(AllNICrimeData_dataframe[AllNICrimeData_dataframe$Location] == "No Location")
AllNICrimeData_dataframe_new <- AllNICrimeData_dataframe[!rowSums(AllNICrimeData_dataframe[4] == "No Location"),]
random_crime_samples <- sample_n(AllNICrimeData_dataframe_new, 1000)
head(random_crime_samples)
str(random_crime_sample)




