#testing your code#

rm(list=ls())

library(lubridate)
#use install.packages to install lubridate
install.packages(c("lubridate"))
#package INSTALLED


#create a function. The names of the arguments for your function will be in parentheses. Everything in curly brackets will be run each time the function is run.
assert <- function(statement,err.message){
  #if evaluates if a statement is true or false for a single item
  if(statement == FALSE){
    print(err.message)
  }
  
}

#check how the statement works
#evaluate a false statement
assert(1 == 2, "error: unequal values")

#evaluate a true statement
assert(2 == 2, "error: unequal values")
#set up assert to check if two vectors are the same length
a <- c(1,2,3,4)
b <- c(8,4,5)
assert(length(a) == length(b), "error: unequal length")

####getting sensor info#####

#read in the data file
#skip the first 3 rows since there is additional column info
#specify the the NA is designated differently
datW <- read.csv("Z:\\stabibian\\github\\data\\bewkes\\bewkes_weather.csv",
                 #skip=3 skips the first three rows in the data set; na.strings=c("#N/A) treats all missing values as NA
                 na.strings=c("#N/A"), skip=3, header=FALSE) #header=FALSE tells it not to treat the first row as the column names; header=TRUE makes first row the column names
#preview data
print(datW[1,])

#get sensor info from file
# this data table will contain all relevant units
sensorInfo <-   read.csv("Z:\\stabibian\\github\\data\\bewkes\\bewkes_weather.csv",
                         na.strings=c("#N/A"), nrows=2)

print(sensorInfo)

#get column names from sensorInfo table
# and set weather station colnames  to be the same
colnames(datW) <- colnames(sensorInfo)
#preview data
print(datW[1,])

#convert to standardized format
#date format is m/d/y
dates <- mdy_hm(datW$timestamp, tz= "America/New_York")

#calculate day of year
datW$doy <- yday(dates)
#calculate hour in the day
datW$hour <- hour(dates) + (minute(dates)/60)
#calculate decimal day (DD) of year
datW$DD <- datW$doy + (datW$hour/24)
#quick preview of new date calculations
datW[1,]

###CHECKING MISSING DATA###

#see how many values have missing data for each sensor observation
#air temperature
length(which(is.na(datW$air.temperature)))

#wind speed
length(which(is.na(datW$wind.speed)))

#precipitation
length(which(is.na(datW$precipitation)))

#soil temperature
length(which(is.na(datW$soil.moisture)))

#soil moisture
length(which(is.na(datW$soil.temp)))


###SETTING UP TESTS FOR QA/QC###

#make a plot with filled in points (using pch)
#line lines
plot(datW$DD, datW$air.temperature, pch=19, type="b", xlab = "Day of Year",
     ylab="Air temperature (degrees C)")

#the first argument is a logical statement to be evaluated as true or false on a vector
#the second argument is the value that my air.tempQ1 column will be given if the statement
#is true. The last value is the value that will be given to air.tempQ1 if the statement is false.
#In this case it is just given the air temperature value
datW$air.tempQ1 <- ifelse(datW$air.temperature < 0, NA, datW$air.temperature)

###checking for realistic values###

#check the values at the extreme range of the data
#and throughout the percentiles
quantile(datW$air.tempQ1)

#look at days with really low air temperature
datW[datW$air.tempQ1 < 8,]  

#look at days with really high air temperature
datW[datW$air.tempQ1 > 33,]  

#plot precipitation and lightning strikes on the same plot
#normalize lighting strikes to match precipitation
lightscale <- (max(datW$precipitation)/max(datW$lightning.acvitivy)) * datW$lightning.acvitivy

#make the plot with precipitation and lightning activity marked
#make it empty to start and add in features
plot(datW$DD , datW$precipitation, xlab = "Day of Year", ylab = "Precipitation & lightning",
     type="n")
#plot precipitation points only when there is precipitation 
#make the points semi-transparent
points(datW$DD[datW$precipitation > 0], datW$precipitation[datW$precipitation > 0],
       col= rgb(95/255,158/255,160/255,.5), pch=15)        

#plot lightning points only when there is lightning     
points(datW$DD[lightscale > 0], lightscale[lightscale > 0],
       col= "tomato3", pch=19)

#QUESTION 5#
assert(length(lightscale) == nrow(datW), "error: unequal length")

#filter out storms in wind and air temperature measurements
# filter all values with lightning that coincides with rainfall greater than 2mm or only rainfall over 5 mm.    
#create a new air temp column
datW$air.tempQ2 <- ifelse(datW$precipitation  >= 2 & datW$lightning.acvitivy >0, NA,
                          ifelse(datW$precipitation > 5, NA, datW$air.tempQ1))

#QUESTION 6#

#copy data over to new column
datW$wind.speedQ1 <- datW$wind.speed

#creating storm conditions variable -- using the same values as the new air temp column
storm_conditions <- (datW$precipitation >= 2 & datW$lightning.acvitivy > 0 | (datW$precipitation > 5))

#filtering suspect wind measurements to NA
datW$wind.speedQ2 <- ifelse(storm_conditions, NA, datW$wind.speedQ1)

#is.na identifies NA values; TRUE indicates the presence of NA value and FALSE indicates a non-missing value
#verifying that the filtering did what I wanted it to do
expected_na <- is.na(datW$wind.speed) | storm_conditions

#identical() tests whether two R objects are exactly equal -- checks for equality in content and attributes and returns a boolean (TRUE if objects are identical)
#tests if there is equality across all rows
assert(identical(is.na(datW$wind.speedQ2), expected_na), "error: wind.speedQ2 NA does not match")

#checks to see if lengths are still the same (like in the other question)
assert(length(datW$wind.speedQ2) == nrow(datW), "error: unequal length: wind speedQ2 vs datW rows")

#plotting the new filtered wind speed dataset
#pch is plotting character
plot(datW$DD, datW$wind.speedQ2, type = "b", pch = 19, xlab = "Day of Year", ylab = "Wind speed (m/s)")

####QUESTION 7####

#checking soil temp and moisture measurements to see if they are reliable

#With() evaluates an expression within a specified data environment -- simplifies code by allowing direct access to columns or elements data objectithout repeatedly prefixing them with the data objects name = shortcut 
#setting same conditions as in the previous question
#storm mask
storm <- with(datW, precipitation >= 2 & lightning.acvitivy > 0 | precipitation > 5)
storm[is.na(storm)] <- FALSE #needs the FALSE otherwise ifelse will show up in the mask

#filtering air temp for storms
datW$.tempQ2 <- ifelse(storm, NA, datW$air.temperature)

#testing storm are NA now
assert(identical(is.na(datW$air.tempQ2), is.na(datW$air.temperature) | storm), "air.tempQ2 NA mismatch")

#is.finite() determines whether elements of a vector are finite numeric values; TRUE indicates a finite value and FALSE indicates an infinite value or NA (but NA in this case)
#uses finite pairs to avoid "need finite ylim" -- kind of confused by this - substack*(come back to)
x<- is.finite(datW$DD) & is.finite(datW$air.tempQ2)
assert(any(x), "no finite (DD, air.tempQ2) pairs to plot")

#plotting data
plot(datW$DD[x], datW$air.tempQ2[x], type = "b", pch = 19, xlab = "Day of Year", ylab = "Air Temperature (C)")

#####QUESTION 8#####

#creating table with average air temperature, wind speed, soil moisture, and soil temperature
#calculating total precipitation
#indicating how many observations went into the calculations and the time period of measurement

#using the mean function to calculate the averages for air temp, wind speed, soil temp, and soil moisture

avg_air_temp <- mean(datW$air.tempQ2,na.rm = TRUE)
avg_wind_speed <- mean(datW$wind.speedQ2, na.rm = TRUE)
avg_soil_temp <- mean(datW$soil.temp, na.rm = TRUE)
avg_soil_moist <- mean(datW$soil.moisture, na.rm = TRUE)

#using the sum function to determine how many observations went into the calculation



