#Juanxi Xue
rm(list=ls())

#Question 1
library(readxl)
#read may14 data, na strings are "NA"

may14 <- read_excel("BAIS alumni.xlsx", na = c("NA",""," "))
may20 <- read.csv("BAIS alumni_05-2020.csv", na.strings = c("NA",""," "))
dec20 <- read.csv("BAIS alumni_12-2020.csv", na.strings = c("NA",""," "))


names(may14)[names(may14) == "Grad\r\nMonth"] <- "grad_month"
names(may14)[names(may14) == "Grad\r\nYear"] <- "grad_year"
names(may14)[names(may14) == "My \r\nConn"] <- "my_conn"
names(may14)[names(may14) == "Job Title"] <- "job_title"
names(may14)[names(may14) == "Current City"] <- "current_city"
names(may14)[names(may14) == "Current State"] <- "current_state"
names(may14)[names(may14) == "Home Country"] <- "home_country"

names(may20)[names(may20) == "Grad.Month"] <- "grad_month"
names(may20)[names(may20) == "Grad.Year"] <- "grad_year"
names(may20)[names(may20) == "My..Conn"] <- "my_conn"
names(may20)[names(may20) == "Job.Title"] <- "job_title"
names(may20)[names(may20) == "Current.City"] <- "current_city"
names(may20)[names(may20) == "Current.State"] <- "current_state"
names(may20)[names(may20) == "Home.Country"] <- "home_country"

names(dec20)[names(dec20) == "Grad.Month"] <- "grad_month"
names(dec20)[names(dec20) == "Grad.Year"] <- "grad_year"
names(dec20)[names(dec20) == "Job.Title"] <- "job_title"
names(dec20)[names(dec20) == "Current.City"] <- "current_city"
names(dec20)[names(dec20) == "Current.State"] <- "current_state"
names(dec20)[names(dec20) == "Home.Country"] <- "home_country"
names(dec20)[names(dec20) == "My..Conn"] <- "my_conn"


#merge
alumni <- merge(may14, may20, all = TRUE)
BAIS_alumni <- merge(alumni, dec20, all = TRUE)
BAIS_alumni <- BAIS_alumni[order(names(BAIS_alumni))]

#########################
#Question 2
#how many student graduated from may 2014 to dec 2020
nrow(BAIS_alumni)
#how many student has been contacted
nrow(subset(BAIS_alumni, my_conn == 3))
paste(as.character((nrow(subset(BAIS_alumni, my_conn == 3))/nrow(BAIS_alumni))*100), "%")

#########################
#Question 3
library(ggplot2)
ggplot(BAIS_alumni, aes(x = grad_year)) + geom_bar() + scale_fill_brewer(palette = "Set1") + labs(title = "BAIS Alumni Graduation Year", x = "Graduation Year", y = "Number of Alumni") + theme(legend.position = "bottom")

#########################
#Question 4
library(dplyr)
#summarize table
top_5_state <- BAIS_alumni %>% group_by(current_state) %>% summarise(n = n()) %>% arrange(desc(n)) %>% head(6) %>% mutate(percentage = paste0(round(n/sum(n)*100, 2), "%"))
#remove the NA row
subset(top_5_state, current_state != "NA")

#########################
#Question 5
alumni_state <- subset(BAIS_alumni, current_state != "NA")
alumni_state$current_state <- ifelse((nchar(alumni_state$current_state) == 2), alumni_state$current_state, "Intl")

#create a bar chart
ggplot(alumni_state, aes(x = current_state)) + geom_bar() + scale_fill_brewer(palette = "Set1") + labs(title = "BAIS Alumni Current State", x = "Current State", y = "Number of Alumni") + theme(legend.position = "bottom")

#########################
#Question 6
#subset the bais major only
BAIS_alumni_BA <- subset(BAIS_alumni, Major1 == "Business Analytics")
BAIS_alumni_BA <- BAIS_alumni_BA[is.na(BAIS_alumni_BA$Major2) & is.na(BAIS_alumni_BA$Minor1) & is.na(BAIS_alumni_BA$Certificate1),]
BA_only_student <- nrow(BAIS_alumni_BA)
#subset when Major2 != NA
BAIS_alumni_BA2 <- subset(BAIS_alumni, Major2 != "NA" | Minor1 != "NA" | Certificate1 != "NA")
multi_major_student <- nrow(BAIS_alumni_BA2)
#create a pie chart with legend 
pie(c(BA_only_student, multi_major_student), labels = c("BA Only", "Multi Major, Minor, or Certificate"), col = c("red", "blue"), main = "BAIS Alumni Major")


#########################
#Question 7

######Major######
#combine second and third major
major2 <- subset(BAIS_alumni, Major2 != "NA")
major3 <- subset(BAIS_alumni, Major3 != "NA")
#drop major 2 column in major 3
major3 <- major3[, -which(names(major3) == "Major2")]
names(major3)[names(major3) == "Major3"] <- "Major2"
second_major <- merge(major2, major3, all = TRUE)
#summarize table
second_major <- second_major %>% group_by(Major2) %>% summarise(n = n()) %>% arrange(desc(n)) %>% head(5)
#create a bar chart
ggplot(second_major, aes(x = Major2, y = n)) + geom_bar(stat = "identity") + scale_fill_brewer(palette = "Set1") + labs(title = "BAIS Alumni Second Major", x = "Second Major", y = "Number of Alumni") + theme(legend.position = "bottom")

######Minor######
#combine first, second and third minor
minor1 <- subset(BAIS_alumni, Minor1 != "NA")
minor2 <- subset(BAIS_alumni, Minor2 != "NA")
minor3 <- subset(BAIS_alumni, Minor3 != "NA")
#drop minor 2 column in minor 3
minor3 <- minor3[, -which(names(minor3) == "Minor2")]
minor3 <- minor3[, -which(names(minor3) == "Minor1")]
names(minor3)[names(minor3) == "Minor3"] <- "Minor1"
minor2 <- minor2[, -which(names(minor2) == "Minor1")]
minor2 <- minor2[, -which(names(minor2) == "Minor2")]
names(minor2)[names(minor2) == "Minor3"] <- "Minor1"
minor <- merge(minor1, minor2, all = TRUE)
minor <- merge(minor, minor3, all = TRUE)
#summarize table
minor <- minor %>% group_by(Minor1) %>% summarise(n = n()) %>% arrange(desc(n)) %>% head(5)
#create a bar chart
ggplot(minor, aes(x = Minor1, y = n)) + geom_bar(stat = "identity") + scale_fill_brewer(palette = "Set1") + labs(title = "BAIS Alumni Minor", x = "Minor", y = "Number of Alumni") + theme(legend.position = "bottom")

######Certificate######
#combine first, second and third certificate
certificate1 <- subset(BAIS_alumni, Certificate1 != "NA")
certificate2 <- subset(BAIS_alumni, Certificate2 != "NA")
certificate3 <- subset(BAIS_alumni, Certificate3 != "NA")
#drop certificate 2 column in certificate 3
certificate3 <- certificate3[, -which(names(certificate3) == "Certificate2")]
certificate3 <- certificate3[, -which(names(certificate3) == "Certificate1")]
names(certificate3)[names(certificate3) == "Certificate3"] <- "Certificate1"
certificate2 <- certificate2[, -which(names(certificate2) == "Certificate1")]
certificate2 <- certificate2[, -which(names(certificate2) == "Certificate2")]
names(certificate2)[names(certificate2) == "Certificate3"] <- "Certificate1"
certificate <- merge(certificate1, certificate2, all = TRUE)
certificate <- merge(certificate, certificate3, all = TRUE)
#summarize table
certificate <- certificate %>% group_by(Certificate1) %>% summarise(n = n()) %>% arrange(desc(n)) %>% head(5)
#create a bar chart
ggplot(certificate, aes(x = Certificate1, y = n)) + geom_bar(stat = "identity") + scale_fill_brewer(palette = "Set1") + labs(title = "BAIS Alumni Certificate", x = "Certificate", y = "Number of Alumni") + theme(legend.position = "bottom")

