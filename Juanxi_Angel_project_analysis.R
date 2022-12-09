#Data Wrangling Final Project
#Angel C, Juanxi X.


#### 1. Stream in and filter
#### 2. Original Data Processing and Merging
#### 3. Data Imputation and Variable Creation
#### 4. Data Visualization and analysis

rm(list = ls())
library(dplyr)

########################################
#### 1. Stream in and filter
weather <- read.csv("Chicago_weather.csv", header = TRUE, sep = ",", stringsAsFactors = FALSE, na.strings = "")
cta <- read.csv("CTA_ridership.csv", header = TRUE, sep = ",", stringsAsFactors = FALSE)
holiday_2022 <- read.csv("holiday2022.csv", header = TRUE, sep = ",", stringsAsFactors = FALSE)
holiday <- read.csv("US_holiday.csv", header = TRUE, sep = ",", stringsAsFactors = FALSE)
#format dates
cta$service_date <- as.POSIXct(cta$service_date, format = "%m/%d/%Y")
holiday$Date <- as.POSIXct(holiday$Date, format = "%Y-%m-%d")
holiday_2022$date <- as.POSIXct(holiday_2022$date, format = "%m/%d/%Y")
weather$date <- weather$Measurement.Timestamp
weather$date <- substr(weather$date, 1, 10)
weather$date <- as.POSIXct(weather$date, format = "%m/%d/%Y")
#filter dates
holiday <- holiday[holiday$Date >= "2015-05-26" & holiday$Date <= "2022-07-31",]
cta <- cta[cta$service_date >= "2015-05-26" & cta$service_date <= "2022-07-31",]

########################################
#### 2. Original Data Processing and Merging
#change column names
names(weather)[names(weather) == "Air.Temperature"] <- "temperature"
names(weather)[names(weather) == "Rain.Intensity"] <- "rain"
names(weather)[names(weather) == "Wind.Speed"] <- "wind_speed"
names(cta)[names(cta) == "service_date"] <- "date"
names(holiday)[names(holiday) == "Holiday"] <- "holiday"
names(holiday)[names(holiday) == "Date"] <- "date"
#select columns
weather <- weather[,c("date", "temperature", "rain", "wind_speed")]
holiday <- holiday[,c("date", "holiday")]
#clean out weather data
weather <- na.omit(weather)
weather <- group_by(weather, date) %>% summarise(temperature = mean(temperature), rain = sum(rain), wind_speed = mean(wind_speed))
#merge
holiday <- rbind(holiday, holiday_2022)
cta_holiday <- merge(cta, holiday, by = "date", all.x = TRUE)
cta_holiday_weather <- merge(cta_holiday, weather, by = "date", all.x = TRUE)

########################################
#### 3. Data Imputation and Variable Creation
#Impute empty values in weathers data
cta_holiday_weather$temperature[is.na(cta_holiday_weather$temperature)] <- 
  (cta_holiday_weather$temperature[is.na(cta_holiday_weather$temperature) - 5] + 
     cta_holiday_weather$temperature[is.na(cta_holiday_weather$temperature) + 5]) / 2

cta_holiday_weather$rain[is.na(cta_holiday_weather$rain)] <- 
  (cta_holiday_weather$rain[is.na(cta_holiday_weather$rain) - 5] + 
     cta_holiday_weather$rain[is.na(cta_holiday_weather$rain) + 5]) / 2

cta_holiday_weather$wind_speed[is.na(cta_holiday_weather$wind_speed)] <-
    (cta_holiday_weather$wind_speed[is.na(cta_holiday_weather$wind_speed) - 5] + 
         cta_holiday_weather$wind_speed[is.na(cta_holiday_weather$wind_speed) + 5]) / 2

#create new variables rain_type and wind_type 
#Rain Weather Type: less than 10 rain is clear, between 10 and 40 is drizzle, between 40 and 80 is rain, more than 80 is heavy rain
cta_holiday_weather$rain_type <- ifelse(cta_holiday_weather$rain > 80, "heavy rain", 
                                   ifelse(cta_holiday_weather$rain >= 40 & cta_holiday_weather$rain < 80, "rain", 
                                   ifelse(cta_holiday_weather$rain >= 10 & cta_holiday_weather$rain < 40, "drizzle", "clear")))
#Wind Weather Type: less than 10 is calm, between 10 and 20 is light breeze, between 20 and 30 is breeze, between 30 and 40 is strong breeze, between 40 and 50 is gale, more than 50 is storm
cta_holiday_weather$wind_type <- ifelse(cta_holiday_weather$wind_speed < 10, "calm", 
                                   ifelse(cta_holiday_weather$wind_speed >= 10 & cta_holiday_weather$wind_speed < 20, "light breeze", 
                                   ifelse(cta_holiday_weather$wind_speed >= 20 & cta_holiday_weather$wind_speed < 30, "breeze", 
                                   ifelse(cta_holiday_weather$wind_speed >= 30 & cta_holiday_weather$wind_speed < 40, "strong breeze", 
                                   ifelse(cta_holiday_weather$wind_speed >= 40 & cta_holiday_weather$wind_speed < 50, "gale", "storm")))))
#create a month column
cta_holiday_weather$month <- substr(cta_holiday_weather$date, 6, 7)
#create a year column
cta_holiday_weather$year <- substr(cta_holiday_weather$date, 1, 4)




########################################
#### 4. Data Visualization and analysis
library(ggplot2)

cta_holiday_weather$year <- substr(cta_holiday_weather$date, 1, 4)
cta_holiday_weather$month <- substr(cta_holiday_weather$date, 6, 7)

#group by month, take average
cta_holiday_weather_avg_month <- group_by(cta_holiday_weather, month) %>% summarise(total_rides = mean(total_rides))
#Monthly Ridership Graph
ggplot(cta_holiday_weather_avg_month, aes(x = month, y = total_rides, fill = month)) + geom_bar(stat = "identity") + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + labs(x = "Month", y = "Average Riders", title = "Average Riders by Month")

#Annual Ridership Graph
cta_holiday_weather_avg_year <- group_by(cta_holiday_weather, year) %>% summarise(total_rides = mean(total_rides))
ggplot(cta_holiday_weather_avg_year, aes(x = year, y = total_rides, fill = year)) + geom_bar(stat = "identity") + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + labs(x = "Year", y = "Average Riders", title = "Average Riders by Year")

#group by day_type
cta_holiday_weather_day_type <- group_by(cta_holiday_weather, day_type) %>% summarise(total_rides = mean(total_rides))
#Day Type
ggplot(cta_holiday_weather_day_type, aes(x = day_type, y = total_rides, fill = day_type)) + geom_bar(stat = "identity") + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + labs(x = "Day Type", y = "Average Riders", title = "Average Riders per Day Type") + scale_x_discrete(labels = c("Saturday","Sunday/Holiday", "Weekday"))

#group by rain_type
cta_holiday_weather_rain_type <- group_by(cta_holiday_weather, rain_type) %>% summarise(total_rides = mean(total_rides))
#Rain type
ggplot(cta_holiday_weather_rain_type, aes(x = rain_type, y = total_rides, fill = rain_type)) + geom_bar(stat = "identity") + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + labs(x = "Rain Type", y = "Average Riders", title = "Average Riders per Rain Type") + scale_x_discrete(labels = c("Clear","Drizzle", "Rain", "Heavy Rain"))