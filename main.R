#Data Wrangling Final Project
#Angel C, Juanxi X.


#### 1. Stream in the data
#### 2. Clean the dates
#### 3. Change column names
#### 4. Clean weather data 
#### 5. Merge data

rm(list = ls())
library(dplyr)

########################################
#### 1. Stream in the data
weather <- read.csv("Chicago_weather.csv", header = TRUE, sep = ",", stringsAsFactors = FALSE, na.strings = "")
cta <- read.csv("CTA_ridership.csv", header = TRUE, sep = ",", stringsAsFactors = FALSE)
#format service_date to use %m/%d/%Y format
cta$service_date <- as.POSIXct(cta$service_date, format = "%m/%d/%Y")
#filter the data from 2015-05-26 to 2021-12-31
cta <- cta[cta$service_date >= "2015-05-26" & cta$service_date <= "2021-12-31",]
holiday <- read.csv("US_holiday.csv", header = TRUE, sep = ",", stringsAsFactors = FALSE)

holiday$Date <- as.POSIXct(holiday$Date, format = "%Y-%m-%d")
holiday <- holiday[holiday$Date >= "2015-05-26" & holiday$Date <= "2021-12-31",]

########################################
#### 2. Clean the dates
weather$date <- weather$Measurement.Timestamp
weather$date <- substr(weather$date, 1, 10)
weather$date <- as.POSIXct(weather$date, format = "%m/%d/%Y")
########################################
#### 3. Change column names
#weather
names(weather)[names(weather) == "Air.Temperature"] <- "temperature"
names(weather)[names(weather) == "Interval.Rain"] <- "rain"
names(weather)[names(weather) == "Wind.Speed"] <- "wind_speed"
#cta
names(cta)[names(cta) == "service_date"] <- "date"
#holiday
names(holiday)[names(holiday) == "Holiday"] <- "holiday"
names(holiday)[names(holiday) == "Date"] <- "date"
holiday <- holiday[,c("date", "holiday")]

########################################
#### 4. Clean weather data
weather <- weather[,c("date", "temperature", "rain", "wind_speed")]
#Weather data clean
weather <- na.omit(weather)
#group by 
weather <- group_by(weather, date) %>% summarise(temperature = mean(temperature), rain = mean(rain), wind_speed = mean(wind_speed))

###Merge
cta_holiday <- merge(cta, holiday, by = "date", all.x = TRUE)
cta_holiday_weather <- merge(cta_holiday, weather, by = "date", all.x = TRUE)

#IF THE TEMPERATURE IS NA, TAKE THE 5 days before and after and take the average
cta_holiday_weather$temperature[is.na(cta_holiday_weather$temperature)] <- 
  (cta_holiday_weather$temperature[is.na(cta_holiday_weather$temperature) - 5] + 
     cta_holiday_weather$temperature[is.na(cta_holiday_weather$temperature) + 5]) / 2

cta_holiday_weather$rain[is.na(cta_holiday_weather$rain)] <- 
  (cta_holiday_weather$rain[is.na(cta_holiday_weather$rain) - 5] + 
     cta_holiday_weather$rain[is.na(cta_holiday_weather$rain) + 5]) / 2

cta_holiday_weather$wind_speed[is.na(cta_holiday_weather$wind_speed)] <-
    (cta_holiday_weather$wind_speed[is.na(cta_holiday_weather$wind_speed) - 5] + 
         cta_holiday_weather$wind_speed[is.na(cta_holiday_weather$wind_speed) + 5]) / 2
# #change Measurement.Timestamp to use %m/%d/%Y format
# weather$date <- as.POSIXct(weather$date, format = "%Y-%m-%d")
# #remove the "00:00:00" from the date
# weather$date <- as.Date(weather$date)


#take first 10 characters in each row of date
#weather$date <- substr(weather$date, 1, 10)
#format date

#filter cta data from 5/26/2015 to 12/31/2021
#cta <- cta[cta$date >= "2015-05-26" & cta$date <= "2021-12-31",]
#show the data after 5/26/2015 in cta
#cta <- cta[cta$date >= "2015-05-26",]