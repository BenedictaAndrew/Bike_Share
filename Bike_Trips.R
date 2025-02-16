# Loading the necessary libraries
library(tidyverse)
library(dplyr)
library(conflicted)
library(lubridate)
library(ggplot2)

# Specify preference for the "filter" function from dplyr
conflict_prefer("filter", "dplyr")

#Loading the data sets
trip_2019<-read.csv("C:/Users/Benedicta/Documents/cousera/Portfoilio/Trip-2019.csv")
trip_2020<-read.csv("C:/Users/Benedicta/Documents/cousera/Portfoilio/Divvy_Trips_2020_Q1.csv")

#Wrangle and combine in a single file
# Compare column names each of the files first,
# While the names don't have to be in the same order, they DO need to--
#match perfectly before we can use a command to join them into one file
colnames(trip_2019)
colnames(trip_2020)

exists("trip_2019") #Checking if the dataset exists
str(trip_2019)  # TRUE i forgot to add read.csv whike loading the data
# Am loading again before checking the colnames

view(trip_2019)
view(trip_2020)

# Rename columns to make them consistent with q1_2020
trip_2019 <- rename(trip_2019
                   ,ride_id = trip_id
                   ,rideable_type = bikeid
                   ,started_at = start_time
                   ,ended_at = end_time
                   ,start_station_name = from_station_name
                   ,start_station_id = from_station_id
                   ,end_station_name = to_station_name
                   ,end_station_id = to_station_id
                   ,member_casual = usertype)

# Convert ride_id and rideable_type to character data type
trip_2019 <- trip_2019 %>%
  mutate(ride_id = as.character(ride_id),
         rideable_type = as.character(rideable_type))

str(trip_2019)

#Stacking the individual df into one big data frame
all_trips<- bind_rows(trip_2019,trip_2020)


colnames(all_trips)

#Remove the gender,birth,lat,lng and tripduration columns as they did not exist in 2020
all_trips <- all_trips %>%
select(-birthyear, -start_lat, -start_lng, -gender, -end_lat, -tripduration,-end_lng)

#Inspect the new table that has been created.
head(all_trips)
nrow(all_trips)
view(all_trips)
str(all_trips)
dim(all_trips)
summary(all_trips)

#Viewing the objects in the member_casual 
table(all_trips$member_casual)

#Consolidate the column member_casual, replacing subscriber to member and
# customer to casuals.
all_trips<-all_trips%>%
mutate(member_casual=recode(member_casual,
                            "Subscriber"="member",
                            "Customer"="casual"))
#checking the objects in member_casual is replaced.
table(all_trips$member_casual)

# convert started_at and ended_at using the format "YYYY-MM-DD HH:MM:SS"

all_trips <- all_trips %>%
  mutate(
    started_at = ymd_hms(started_at),     # Convert to date-time format
    ended_at = ymd_hms(ended_at),         # Convert to date-time format
    ride_date = as.Date(started_at),      # Extract date only
    ride_month = month(started_at),       # Extract month
    ride_day = day(started_at),           # Extract day
    ride_year = year(started_at),         # Extract year
    days_of_week = wday(started_at, label = TRUE, abbr = FALSE))  # Extract day of week as full name


all_trips <- all_trips%>%
mutate(trip_duration = as.numeric(difftime(ended_at, started_at, units = "secs"))
)

str(all_trips) 

#checking for trip_duration that shows anomalies.
short_trips<-all_trips%>%
  filter(trip_duration < 1)

view(short_trips)

#Removing these anomalies
all_trips_2 <- all_trips %>%
  filter(!(trip_duration < 0 | start_station_name=='HQ QR'))

#checking for NA 
sum(is.na(all_trips_2$trip_duration))
sum(all_trips_2$start_station_name== "HQ QR")

view(all_trips_2)
head(all_trips_2)  # Check the first few rows
str(all_trips_2)

#Descriptive analysis of the trip_duration
mean(all_trips_2$trip_duration)
min(all_trips_2$trip_duration)
median(all_trips_2$trip_duration)
max(all_trips_2$trip_duration)

#summarizes the ride_length for each unique value in member_casual.
aggregate(all_trips_2$trip_duration ~ all_trips_2$member_casual, FUN = mean)
aggregate(all_trips_2$trip_duration ~ all_trips_2$member_casual, FUN = median)
aggregate(all_trips_2$trip_duration ~ all_trips_2$member_casual, FUN = max)
aggregate(all_trips_2$trip_duration ~ all_trips_2$member_casual, FUN = min)

#Let's put the days of the week in other of days
all_trips_2$days_of_week <- as.character(all_trips_2$days_of_week)
all_trips_2$days_of_week <- ordered(all_trips_2$days_of_week,
  levels=c("Sunday", "Monday","Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))

unique(all_trips_2$day_of_week)

# Now, let's run the average ride time by each day for members vs casual users
aggregate(all_trips_2$trip_duration ~ all_trips_2$member_casual + 
            all_trips_2$days_of_week, FUN = mean)                                             


# analyze ridership data by type and weekday
all_trips_2<-all_trips_2%>%
mutate(weekday=wday(started_at, label = TRUE))%>%
      group_by(member_casual,weekday)%>%
      summarise(ride_number=n()
                ,average_trip=mean(trip_duration))%>%
      arrange(member_casual,weekday)

# Let's visualize the number of rides by rider type      
  ggplot(all_trips_2,aes(x=weekday, y=ride_number, fill=member_casual))+ 
           geom_col(position="dodge")+
          scale_fill_manual(values = c("casual" = "red", "member" = "darkgreen"))


# Let's create a visualization for average duration by rider type
ggplot(all_trips_2,aes(x=weekday, y=average_trip, fill=member_casual,))+
      geom_col(position="dodge")+
      scale_fill_manual(values = c("casual" = "lightgreen", "member" = "red"))


                                            
