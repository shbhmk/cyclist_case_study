library(tidyverse)
library(ggplot2)
library(dplyr)
library(lubridate)
getwd()
setwd("C:/Users/shbhm/Desktop/case_study")
df1 <- read.csv("2021_jan.csv")
df2 <- read.csv("2021_feb.csv")
df3 <- read.csv("2021_mar.csv")
df4 <- read.csv("2021_april.csv")
df5 <- read.csv("2021_may.csv")
df6 <- read.csv("2021_jun.csv")
df7 <- read.csv("2021_jul.csv")
df8 <- read.csv("2021_august.csv")
df9 <- read.csv("2020_sep.csv")
df10 <- read.csv("2020_oct.csv")
df11 <- read.csv("2020_nov.csv")
df12 <- read.csv("2020_dec.csv")
colnames(df1)
colnames(df2)
colnames(df3)
colnames(df4)
colnames(df5)
colnames(df6)
colnames(df7)
colnames(df8)
colnames(df9)
colnames(df10)
colnames(df11)
colnames(df12)
str(df1)
str(df12)
str(df11)
df11 <- mutate(df11, end_station_id = as.character(end_station_id)) 
str(df11)
df11 <- mutate(df11, start_station_id = as.character(start_station_id))
df9 <- mutate(df9, start_station_id = as.character(start_station_id))
df9 <- mutate(df9, end_station_id = as.character(end_station_id))
str(df11)
str(df9)
df10 <- mutate(df10, end_station_id = as.character(end_station_id))
df10 <- mutate(df10, start_station_id = as.character(start_station_id))
df12 <- mutate(df12, start_station_id = as.character(start_station_id))
df12 <- mutate(df12, end_station_id = as.character(end_station_id))
str(df1)
str(df2)
str(df3)
str(df4)
str(df5)
str(df6)
str(df7)
str(df8)
str(df9)
str(df10)
str(df11)
str(df12)
all <- bind_rows(df1,df2,df3,df4,df5,df6,df7,df8,df9,df10,df11,df12)
str(all)
summarise(all)
View(all)
nrow(all)
summary(all)
drop_na(all)
nrow(all)
colnames(all)
unique(all[c("member_casual")])
library(dplyr)
all <- all %>%
  na_if("") %>%
  na.omit()
nrow(all)
colnames(all)
all$date <- as.Date(all$started_at)
all$month <- format(as.Date(all$date), "%m")
all$day <-format(as.Date(all$date), "%d") 
all$year <- format(as.Date(all$date), "%Y")
all$days_of_week <- format(as.Date(all$date), "%A")
all$ended_at <- as.POSIXct(all$ended_at)
all$started_at <- as.POSIXct(all$started_at)
all$ride_length <- difftime(all$ended_at,all$started_at)
str(all)
all$ride_length <- as.numeric(as.character(all$ride_length))
View(all)
unique(all[c("start_station_name")])
unique(all[c("end_station_name")])
unique(all[c("ride_length")])
all2 <-all[!(all$ride_length < 0),]
str(all2)
nrow(all2)
mean(all2$ride_length)
View(all2)
median(all2$ride_length)
max(all2$ride_length)
min(all2$ride_length)
aggregate(all2$ride_length ~ all2$member_casual, FUN= mean)
aggregate(all2$ride_length ~ all2$member_casual, FUN= median)
aggregate(all2$ride_length ~ all2$member_casual, FUN= max)
aggregate(all2$ride_length ~ all2$member_casual, FUN= min)
aggregate(all2$ride_length ~ all2$member_casual + all2$days_of_week, FUN= mean)
all2 %>%
   mutate(weekday = wday(started_at, label = TRUE)) %>%
   group_by(member_casual,weekday) %>%
   summarise(number_of_rides = n(), average_duration = mean(ride_length)) %>%
   arrange(member_casual,weekday)
colnames((all2))  
all2 %>%
  mutate(weekday = wday(started_at, label = TRUE)) %>%
  group_by(member_casual,weekday) %>%
  summarise(number_of_rides = n(), average_duration = mean(ride_length)) %>%
  arrange(member_casual,weekday)
 all2 <- all2 %>%
   mutate(weekday = wday(started_at, label = TRUE))
colnames(all2)                             
all2 %>%
  mutate(weekday = wday(started_at, label = TRUE)) %>%
  group_by(member_casual, weekday) %>%
  summarise(number_of_rides = n(), average_duration = mean(ride_length), options(scipen = n)) %>%
  arrange(member_casual, weekday) %>%
  ggplot(aes(x = number_of_rides, y = weekday)) + geom_col(position = "dodge")

all2 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>% 
  group_by(member_casual, weekday) %>% 
  summarise(number_of_rides = n()
            ,average_duration = mean(ride_length)) %>% 
  arrange(member_casual, weekday)  %>% 
  ggplot(aes(x = weekday, y = average_duration, fill = member_casual)) +
  geom_col(position = "dodge")


write.csv(all2,"cyclist.csv")

