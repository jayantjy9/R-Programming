## Load The required Packages and Libraries
install.packages("tidyverse")
library(tidyverse)

## Load the Dataset
df1 <- read_csv("202101-divvy-tripdata.csv")
df2 <- read_csv("202102-divvy-tripdata.csv")

# combining both dataframe

complete_data <- rbind(df1,df2)

## removing the temporary Dataframe
rm(df1)
rm(df2)

## load and view the Complete data
view(complete_data)
head(complete_data)
nrow(complete_data)
colnames(complete_data)
str(complete_data)
glimpse(complete_data)

## changing the data type
complete_data$started_at <- as.Date(complete_data$started_at,format = "%d-%m-%y")
complete_data$ended_at <- as.Date(complete_data$ended_at,format = "%d-%m-%y")



summary(complete_data)

# cleaning data taking only ride_length above 0
complete_data <-complete_data %>% 
  filter(ride_length>0)
print("Cleaned data Count:")
nrow(complete_data)

# Descriptive Analysis

# Overall data
complete_data %>% 
  summarize(mean(ride_length),max(ride_length))

# Based on member and weekdays
complete_data %>% 
  group_by(member_casual, day_of_week) %>% 
  summarise(count=n(),min(ride_length),max(ride_length),mean(ride_length),median(ride_length)) %>% 
  arrange(member_casual,count)

# Average ride length by user type and weekdays
complete_data %>% 
  group_by(member_casual,day_of_week) %>% 
  summarise(average_duration = mean(ride_length)) %>% 
  arrange(member_casual,-average_duration)

complete_data %>% 
  group_by(member_casual) %>% 
  summarize(count=n(),total_pop=nrow(complete_data),share=count/total_pop*100,mean(ride_length))

## Visualization

complete_data %>% 
  group_by(member_casual,day_of_week) %>% 
  summarise(no_of_rides= n()) %>% 
  ggplot(aes(y=day_of_week,x=no_of_rides, fill=member_casual))+geom_col()+
  labs(title = "Weekwise count of users")
  

# Average ride length of users

complete_data %>% 
  group_by(member_casual,day_of_week) %>%
  summarise(avg_duration = mean(ride_length)) %>%
  ggplot(mapping = aes(x=avg_duration, y=day_of_week, fill=member_casual))+geom_col()+
  labs(title="Average duration of ride length")


complete_data %>% 
  group_by(member_casual,day_of_week) %>%
  summarise(min_duration = min(ride_length)) %>%
  ggplot(mapping = aes(x=min_duration, y=day_of_week, fill=member_casual))+geom_col()+
  labs(title="Minimum duration of ride length")

complete_data %>% 
  group_by(member_casual,day_of_week) %>%
  summarise(max_duration = max(ride_length)) %>%
  ggplot(mapping = aes(x=max_duration, y=day_of_week, fill=member_casual))+geom_col()+
  labs(title="Maximum duration of ride length")


