install.packages(c("tidyverse","lubridate","janitor","skimr","data.table","readr"))

library(tidyverse)
library(lubridate)
library(janitor)
library(skimr)
library(data.table)
library(readr)


# setting the directory 
setwd("C:/Users/Zedi/Downloads/Cyclistic Project/Data")

#Load Data

data_202402 <- read.csv("C:/Users/Zedi/Downloads/Cyclistic Project/Data/202402-divvy-tripdata.csv")
data_202403 <- read.csv("C:/Users/Zedi/Downloads/Cyclistic Project/Data/202403-divvy-tripdata.csv")
data_202404 <- read.csv("C:/Users/Zedi/Downloads/Cyclistic Project/Data/202404-divvy-tripdata.csv")
data_202405 <- read.csv("C:/Users/Zedi/Downloads/Cyclistic Project/Data/202405-divvy-tripdata.csv")
data_202406 <- read.csv("C:/Users/Zedi/Downloads/Cyclistic Project/Data/202406-divvy-tripdata.csv")
data_202407 <- read.csv("C:/Users/Zedi/Downloads/Cyclistic Project/Data/202407-divvy-tripdata.csv")
data_202408 <- read.csv("C:/Users/Zedi/Downloads/Cyclistic Project/Data/202408-divvy-tripdata.csv")
data_202409 <- read.csv("C:/Users/Zedi/Downloads/Cyclistic Project/Data/202409-divvy-tripdata.csv")
data_202410 <- read.csv("C:/Users/Zedi/Downloads/Cyclistic Project/Data/202410-divvy-tripdata.csv")
data_202411 <- read.csv("C:/Users/Zedi/Downloads/Cyclistic Project/Data/202411-divvy-tripdata.csv")
data_202412 <- read.csv("C:/Users/Zedi/Downloads/Cyclistic Project/Data/202412-divvy-tripdata.csv")
data_202501 <- read.csv("C:/Users/Zedi/Downloads/Cyclistic Project/Data/202501-divvy-tripdata.csv")
data_202502 <- read.csv("C:/Users/Zedi/Downloads/Cyclistic Project/Data/202502-divvy-tripdata.csv")
data_202503 <- read.csv("C:/Users/Zedi/Downloads/Cyclistic Project/Data/202503-divvy-tripdata.csv")

#consolidating all the trips data for last 12 months
consolidate_trips <- bind_rows(data_202404,data_202405,data_202406,data_202407,data_202408,data_202409,data_202410,data_202411,data_202412,data_202501,data_202502,data_202503)

#formatting time and creating custom columns 

consolidate_trips <- consolidate_trips %>%
  mutate(started_at = ymd_hms(started_at),
         ended_at = ymd_hms(ended_at),
         ride_length = as.numeric(difftime(ended_at,started_at,units = "mins")),
         day_of_week = wday(started_at))

#aggregating the data & Analyzing the data 

summary_stats <- consolidate_trips %>%
  group_by(member_casual) %>%
  
  summarise(mean_ride = mean(ride_length),
            median_ride = median(ride_length),
            max_ride = max(ride_length),
            mode_ride = mode(ride_length),
            total_rides = n()
            )

day_usage <- consolidate_trips %>%
  group_by(member_casual,day_of_week) %>%
  summarise(avg_duration = mean(ride_length),
            ride_count = n())
  

#Visualization

ggplot(day_usage,aes(x = day_of_week, y=avg_duration, fill = member_casual)) +
  geom_col(position = "dodge") +
  labs(title = "Average Ride Duration By Day",
       x = "Week Day", y = "Duration (mins)",
       fill = "User Type") +
  theme_minimal()
       

ggplot(day_usage,aes(x = day_of_week, y=ride_count, fill = member_casual)) +
       geom_col(position = "dodge") +
       labs(title = "Riders Per Day",
       x = "Week Day", y = "Duration (mins)",
       fill = "User Type") +
  theme_minimal()















