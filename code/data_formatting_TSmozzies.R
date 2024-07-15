## code to clean data on mosquito abundance 
## and combine it with climate predictors
## based on code from previous VectorBiTE training

require(dplyr)
require(tidyr)

## mozzie data

Complete_data <- read.csv("../data/Culex_erraticus_walton.csv")

# select only the rows of interest 
Main_data <- select(Complete_data, c("sample_end_date", "sample_value", 
                                     "sample_lat_dd", "sample_long_dd"))

# Make sure the sample end date is in date format
Main_data$sample_end_date <- as.Date(Main_data$sample_end_date, format = "%Y-%m-%d")

# Order by date 
Main_data <- Main_data[order(Main_data$sample_end_date, decreasing=FALSE),]

# We can now create columns for Month/Year and Month 
Main_data$Month_Yr <- format(as.Date(Main_data$sample_end_date), "%Y-%m")
Main_data$Month <- format(as.Date(Main_data$sample_end_date), "%m")

Main_data$MaxTemp <- Main_data$Precip <- NA

## Climate Data

Climate_data <- read.csv("../data/vectorbase_locations_dates_climate.csv")

## We can now populate these columns by matching up the date for each row, 
## and the closest co-ordinates we have in our climate data.

# For each row in Main_data
for (row in 1:nrow(Main_data)){
  
  # extract the date associated with the row 
  date <- as.character(Main_data[row, "sample_end_date"])
  
  # subset the climate data to only those with the same date
  data <- subset(Climate_data, Climate_data$Collection.date.range == date)
  
  if (nrow(data)>0){
    
    # define the lat and long desired
    lat <- as.numeric(Main_data[row, "sample_lat_dd"])
    long <- as.numeric(Main_data[row, "sample_long_dd"])
    
    # find the absolute differences between desired lat/longs to the climate datasets
    x <- abs(data$Initial_latitude - lat)
    y <- abs(data$Initial_longitude - long)
    
    # find the index for which there is the minimum overall difference between lat/longs 
    z<-which(x+y==min(x+y))
    
    # draw out the max temp and place into main data frame 
    Main_data[row, "MaxTemp"] <- data[z, "Max.Temp"]
    Main_data[row, "Precip"] <- data[z, "Precipitation"]
    
  }
  
  else{
    
    # If there aren't any data to extract for a given date, input NAs
    Main_data[row, "MaxTemp"] <- NA
    Main_data[row, "Precip"] <- NA
  } 
}

summary(Main_data) ## looks good

## take averages for each month
Aggregated_data <- aggregate(cbind(sample_value, MaxTemp, Precip) ~ Month_Yr, 
                             data = Main_data, mean)
print(Aggregated_data)

write.csv(Aggregated_data, file="Culex_erraticus_walton_covariates_aggregated.csv", 
          row.names = FALSE)

plot(Aggregated_data$sample_value, type="l")


