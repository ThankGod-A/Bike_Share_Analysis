# To Load packages needed for importing data and getting comprehensive summary 

library(tidyverse)
library(readr)
library(skimr)

# To Import files as Data frame using read.csv() function and 
#assigning variables to each files

DF_Jul_2021 <- read.csv("C:\\users\\ThankGod\\Documents\\202107-divvy-tripdata_1.csv")
DF_Aug_2021 <- read.csv("C:\\users\\ThankGod\\Documents\\202108-divvy-tripdata_1.csv")
DF_Sept_2021 <- read.csv("C:\\users\\ThankGod\\Documents\\202109-divvy-tripdata_1.csv")
DF_Oct_2021 <- read.csv("C:\\users\\ThankGod\\Documents\\202110-divvy-tripdata_1.csv")
DF_Nov_2021 <- read.csv("C:\\users\\ThankGod\\Documents\\202111-divvy-tripdata_1.csv")
DF_Dec_2021 <- read.csv("C:\\users\\ThankGod\\Documents\\202112-divvy-tripdata_1.csv")
DF_Jan_2022 <- read.csv("C:\\users\\ThankGod\\Documents\\202201-divvy-tripdata_1.csv")
DF_Feb_2022 <- read.csv("C:\\users\\ThankGod\\Documents\\202202-divvy-tripdata_1.csv")
DF_March_2022 <- read.csv("C:\\users\\ThankGod\\Documents\\202203-divvy-tripdata_1.csv")
DF_Apr_2022 <- read.csv("C:\\users\\ThankGod\\Documents\\202204-divvy-tripdata_1.csv")
DF_May_2022 <- read.csv("C:\\users\\ThankGod\\Documents\\202205-divvy-tripdata_1.csv")
DF_June_2022 <- read.csv("C:\\users\\ThankGod\\Documents\\202206-divvy-tripdata_1.csv")

# To combine the data frames into one using bind_rows() function.

Bike_rides <- bind_rows(DF_Jul_2021, DF_Aug_2021, DF_Sept_2021, DF_Oct_2021,
                        DF_Nov_2021, DF_Dec_2021, DF_Jan_2022, DF_Feb_2022,
                        DF_Mar_2022, DF_Apr_2022, DF_May_2022, DF_June_2022)

# To understand the structure, shape and comprehensive summary of the 
#dataset.

skim_without_charts(Bike_rides)
glimpse(Bike_rides)
str(Bike_rides)
head(Bike_rides)
tail(Bike_rides)

#_______________DATA PROCESSING_______________#

# To load packages needed for data wrangling

library(dplyr)
library(tidyr)
library(lubridate)
library(janitor)

# To change the datatypes of started_at, ended_at and ride_length
#variables/columns

Bike_rides$started_at <- lubridate::dmy_hm(Bike_rides$started_at)
Bike_rides$ended_at <- lubridate::dmy_hm(Bike_rides$ended_at)
Bike_rides[['ride_length']] <- as.POSIXct(Bike_rides[['ride_length']], 
                                               format = "%H:%M:%S")

# To removed rows with blanks or empty spaces from end_station_name and
#end_station_id

Bike_rides = Bike_rides[Bike_rides$end_station_name != "", ]

# To explore and remove NA in the dataset

colSums(is.na(Bike_rides))
sum(is.na(Test_rides))
Bike_rides <- na.omit(Bike_rides)

# To split date-time columns into two separate columns (date & time) 
#in started_at, ended_at and ride_length columns.

Bike_rides <- separate(Bike_rides, "started_at", into=c('start_date', 
                                     'start_time'), sep = ' ')
Bike_rides <- separate(Bike_rides, "ended_at", into=c('end_date', 
                                      'end_time'), sep = ' ')
Bike_rides <- separate(Bike_rides, "ride_length", into=c('ride_length_date',
                                     'ride_length_time'), sep=' ')

# To Make the columns consistent by renaming it using rename() function

Bike_rides <- Bike_rides %>% 
  rename(bike_type = rideable_type,
         user_type = member_casual)

# To convert start_date and ride_length_time from character to its
# respective datatypes

Bike_rides$start_date <- lubridate::as_date(Bike_rides$start_date)
Bike_rides$ride_length_time<- lubridate::hms(Bike_rides$ride_length_time)

# To have weekday in letters. Created a new variables 'day_of_week_1'
#'using weekdays() function.

Bike_rides$day_of_week_1 <- weekdays(Bike_rides$start_date)

# To removed ride_length_time with negative value and less than 1 
#minute

Bike_rides <- Bike_rides %>% 
  filter(ride_length_time > 0)

#_______________DATA_ANALYSIS_______________#

# 1.To calculate  Mean ride_length?

Bike_rides %>% 
  summarize(Average_Ride = mean(ride_length_time))

# 2. Ride Summary(Mean, Min, Max)?

Bike_rides %>% 
  group_by(user_type) %>% 
  summarise(Average_ride_time = mean(ride_length_time),
            Max_ride_time = max(ride_length_time),
            Min_ride_time = min(ride_length_time))

# 3. To analyze the Mode of day of week?
# Defined a function used to calculate the Mode

Mode <- function(x) {
  uniqx <- unique(x)
  uniqx[which.max(tabulate(match(x, uniqx)))] }

x = Bike_rides$day_of_week_1
y <- Mode(x)
print(y)

#___Alternatively
names(sort(-table(Bike_rides$day_of_week_1)))[1]

# 4. To calculate the average ride_length for members and casual riders.

Bike_rides %>% 
  group_by(user_type) %>% 
  summarise(Average_ride_length = mean(ride_length_time)) %>% 

# 5. To calculate the average ride_length for users by day_of_week. 
#Try columns = day_of_week; Rows = member_casual; Values = Average of ride_length.

Bike_rides %>% #[Not done yet]
  group_by(user_type, day_of_week_1) %>% 
  summarize(Number_of_rides = n(),
            Average_ride_length = mean(ride_length_time)) %>% 
  arrange(day_of_week_1)

# 6.To analyze the number of rides for users by day_of_week

Bike_rides %>% 
  group_by(user_type, day_of_week_1) %>% 
  summarise(Number_of_rides = n())
  
# 7. To analyze the number of rides for users

Bike_rides %>%
  group_by(user_type) %>% 
  summarise(Number_of_Rides = n()) %>% 
  arrange(desc(Number_of_Rides))

# 8.To analyze the number of rides for users per season?  
  
Bike_rides %>%
  group_by(user_type, Season = case_when(
    month(lubridate::as_date(start_date)) == 12 ~ 'Winter',
    month(lubridate::as_date(start_date)) == 01 ~ 'Winter',
    month(lubridate::as_date(start_date)) == 02 ~ 'Winter',
    month(lubridate::as_date(start_date)) == 03 ~ 'Spring',
    month(lubridate::as_date(start_date)) == 04 ~ 'Spring',
    month(lubridate::as_date(start_date)) == 05 ~ 'Spring',
    month(lubridate::as_date(start_date)) == 06 ~ 'Summer',
    month(lubridate::as_date(start_date)) == 07 ~ 'Summer',
    month(lubridate::as_date(start_date)) == 08 ~ 'Summer',
    TRUE ~ ' Fall')) %>% 
  summarize(Number_of_rides = n()) %>% 
  arrange(Season)

# 9. To analyze the number of rides per month

Bike_rides %>% #[4]
  group_by(user_type, Month = case_when(
    month(lubridate::as_date(start_date)) == 01 ~ 'Jan',
    month(lubridate::as_date(start_date)) == 02 ~ 'Feb',
    month(lubridate::as_date(start_date)) == 03 ~ 'Mar',
    month(lubridate::as_date(start_date)) == 04 ~ 'April',
    month(lubridate::as_date(start_date)) == 05 ~ 'May',
    month(lubridate::as_date(start_date)) == 06 ~ 'June',
    month(lubridate::as_date(start_date)) == 07 ~ 'July',
    month(lubridate::as_date(start_date)) == 08 ~ 'Aug',
    month(lubridate::as_date(start_date)) == 09 ~ 'Sept',
    month(lubridate::as_date(start_date)) == 10 ~ 'Oct',
    month(lubridate::as_date(start_date)) == 11 ~ 'Nov',
    TRUE ~ ' Dec')) %>% 
  summarize(Number_of_rides = n()) %>% 
  arrange(desc(Number_of_rides))

# 10. To analyze the most preferred time_of_day

Bike_rides %>% 
  group_by(user_type, time_of_day = case_when(
    start_time >= "06:00" & start_time <= "12:00" ~ "Morning",
    start_time >= "12:00" & start_time <= "16:00" ~ "Afternoon",
    start_time >= "16:00" & start_time <= "19:00" ~ "Evening",
    TRUE ~ "Night")) %>% 
  summarize(Number_of_rides = n()) %>% 
  arrange(desc(Number_of_rides))

# 11. To analyze the most preferred bike type

Bike_rides %>% 
  group_by(bike_type) %>% 
  summarize(Number_of_usage = n()) %>% 
  #arrange(desc(Number_of_usage)) %>% 
  mutate(percentage = round(Number_of_usage/sum(Number_of_usage),4) * 100,
         lab.pos = cumsum(percentage) -0.5*percentage) 

#_______________VISUALIZATION_______________#

# To load packages needed for data exploration

library(ggplot2)
library(scales)
library(ggpubr)
library(ggsci)
library(viridis)
library(RColorBrewer)
library(ggrepel)

# To view the number of rides for users by day_of_week?

Bike_rides %>% 
  group_by(user_type, day_of_week_1) %>% 
  summarise(Number_of_rides = n()) %>% 
  ggplot(aes(reorder(day_of_week_1, Number_of_rides), Number_of_rides, fill = user_type))+
  geom_bar(position = 'dodge', stat = 'identity') +
  labs(title = "Ride by Day_of_Week") +
  theme(panel.spacing = unit(4, "lines"), 
        axis.text.x=element_text(angle=45, hjust=1)) +
  scale_fill_manual(values = c("casual" = "#BE9E6F",
                               "member" = "#2A603B")) +
  theme(legend.justification = c("right", "top")) + 
  theme(legend.background = element_rect(fill="#D9DFE0", 
                                         size=0.5, linetype="solid")) +
  theme(plot.background = element_rect(fill = "#D9DFE0")) +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank()) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.title = element_text(face = "bold")) +
  scale_y_continuous(labels = number)

# To view ride distribution, i.e, the number of rides for users

Pie = Bike_rides %>%
  group_by(user_type) %>% 
  summarise(Number_of_Rides = n()) %>%
  ungroup() %>% 
  arrange(desc(Number_of_Rides)) %>% 
  mutate(percentage = round(Number_of_Rides/sum(Number_of_Rides),4) * 100,
         lab.pos = cumsum(percentage) -0.5*percentage)

  ggplot(data = Pie,
    aes(x = "", y = percentage, fill = user_type)) +
  geom_bar(stat = "identity") + #Plot a stack bar chart
  coord_polar("y", start = 200) +
  geom_text(aes(y = lab.pos, label = paste(percentage, "%", sep = ""))
            , col = "black") +
  ggtitle("Ride Distribution") +
  theme_void() + 
  theme(plot.background = element_rect(fill = "#D9DFE0")) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.title = element_text(face = "bold")) + 
  theme(legend.justification = c("right", "top")) +
  scale_fill_manual(values = c("#BE9E6F", "#2A603B")) 
print(Pie)

#____Donut Chart
ggplot(data = Pie,
       aes(x =2, y = percentage, fill = user_type)) +
  geom_bar(stat = "identity") +
  coord_polar("y", start = 200) +
  geom_text(aes(y = lab.pos, label = paste(percentage, "%", 
                                           sep = "")), col = "black") +
  ggtitle("Ride Distribution") +
  theme_void() + 
  theme(legend.justification = c("right", "top")) +
  scale_fill_manual(values = c("#BE9E6F", "#2A603B")) +
  xlim(0.5, 2.5) + 
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.title = element_text(face = "bold")) +
  theme(plot.background = element_rect(fill = "#D9DFE0"))

# To view the number of rides per season

Bike_rides %>%
  group_by(user_type, Season = case_when(
    month(lubridate::as_date(start_date)) == 12 ~ 'Winter',
    month(lubridate::as_date(start_date)) == 01 ~ 'Winter',
    month(lubridate::as_date(start_date)) == 02 ~ 'Winter',
    month(lubridate::as_date(start_date)) == 03 ~ 'Spring',
    month(lubridate::as_date(start_date)) == 04 ~ 'Spring',
    month(lubridate::as_date(start_date)) == 05 ~ 'Spring',
    month(lubridate::as_date(start_date)) == 06 ~ 'Summer',
    month(lubridate::as_date(start_date)) == 07 ~ 'Summer',
    month(lubridate::as_date(start_date)) == 08 ~ 'Summer',
    TRUE ~ ' Fall')) %>% 
  summarize(Number_of_rides = n()) %>% 
  ggplot(aes(reorder(Season, Number_of_rides), Number_of_rides, fill = user_type))+
  geom_bar(position = 'dodge', stat = 'identity') +
  labs(title = "Rides by Season") +
  theme(panel.spacing = unit(4, "lines"), 
        axis.text.x=element_text(angle=45, hjust=1)) +
  scale_fill_manual(values = c("casual" = "#BE9E6F",
                               "member" = "#2A603B")) +
  theme(legend.justification = c("right", "top")) + 
  theme(legend.background = element_rect(fill="#D9DFE0", 
                                         size=0.5, linetype="solid")) +
  theme(plot.background = element_rect(fill = "#D9DFE0")) +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank()) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.title = element_text(face = "bold")) +
  scale_y_continuous(labels = number)

#  To view the number of rides per month

Bike_rides %>%
  group_by(user_type, Month = case_when(
    month(lubridate::as_date(start_date)) == 01 ~ 'Jan',
    month(lubridate::as_date(start_date)) == 02 ~ 'Feb',
    month(lubridate::as_date(start_date)) == 03 ~ 'Mar',
    month(lubridate::as_date(start_date)) == 04 ~ 'April',
    month(lubridate::as_date(start_date)) == 05 ~ 'May',
    month(lubridate::as_date(start_date)) == 06 ~ 'June',
    month(lubridate::as_date(start_date)) == 07 ~ 'July',
    month(lubridate::as_date(start_date)) == 08 ~ 'Aug',
    month(lubridate::as_date(start_date)) == 09 ~ 'Sept',
    month(lubridate::as_date(start_date)) == 10 ~ 'Oct',
    month(lubridate::as_date(start_date)) == 11 ~ 'Nov',
    TRUE ~ ' Dec')) %>% 
  summarize(Number_of_rides = n()) %>% 
  ggplot(aes(reorder(Month, Number_of_rides), Number_of_rides, fill = user_type))+
  geom_bar(position = 'dodge', stat = 'identity') +
  labs(title = "Rides by Month") +
  theme(panel.spacing = unit(4, "lines"), 
        axis.text.x=element_text(angle=45, hjust=1)) +
  scale_fill_manual(values = c("casual" = "#BE9E6F",
                               "member" = "#2A603B")) +
  theme(legend.justification = c("right", "top")) + 
  theme(legend.background = element_rect(fill="#D9DFE0", 
                                         size=0.5, linetype="solid")) +
  theme(plot.background = element_rect(fill = "#D9DFE0")) +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank()) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.title = element_text(face = "bold")) +
  scale_y_continuous(labels = number)

# To view the most preferred time of day

Bike_rides %>% 
  group_by(user_type, time_of_day = case_when(
    start_time >= "06:00" & start_time <= "12:00" ~ "Morning",
    start_time >= "12:00" & start_time <= "16:00" ~ "Afternoon",
    start_time >= "16:00" & start_time <= "19:00" ~ "Evening",
    TRUE ~ "Night")) %>% 
  summarize(Number_of_rides = n()) %>% 
  arrange(desc(Number_of_rides)) %>% 
  ggplot(aes(reorder(time_of_day, Number_of_rides), Number_of_rides, fill = user_type))+
  geom_bar(position = 'dodge', stat = 'identity') +
  labs(title = "Rides by time_of_day") +
  theme(panel.spacing = unit(4, "lines"), 
        axis.text.x=element_text(angle=45, hjust=1)) +
  scale_fill_manual(values = c("casual" = "#BE9E6F",
                               "member" = "#2A603B")) +
  theme(legend.justification = c("right", "top")) + 
  theme(legend.background = element_rect(fill="#D9DFE0", 
                                         size=0.5, linetype="solid")) +
  theme(plot.background = element_rect(fill = "#D9DFE0")) +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank()) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.title = element_text(face = "bold")) +
  scale_y_continuous(labels = number)

# To view the most preferred bike type

Bike_rides %>% 
  group_by(bike_type) %>% 
  summarize(Number_of_usage = n()) %>% 
  arrange(desc(Number_of_usage)) %>%
  mutate(percentage = round(Number_of_usage/sum(Number_of_usage),4) * 100,
         lab.pos = cumsum(percentage) -0.5*percentage) %>%  
 ggplot(aes(x =2, y = percentage, fill = bike_type)) +
  geom_bar(stat = "identity") +
  coord_polar("y", start = 200) +
  #geom_text(aes(y = lab.pos, label = paste(percentage, "%", 
                                           #sep = "")), col = "black") +
  ggtitle("Bike_type Distribution") +
  theme_void() + 
  theme(legend.justification = c("right", "top")) +
  scale_fill_manual(values = c("#BE9E6F", "#568203", "#05472a")) +
  xlim(0.5, 2.5) + # 0.5 = hsize. smaller d num bigger d hsize
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.title = element_text(face = "bold")) +
  theme(plot.background = element_rect(fill = "#D9DFE0"))+
  theme(legend.key = element_rect(fill = "black", colour = "black"))+
  geom_text(aes(label = paste(percentage, "%", sep = "")),
            position = position_stack(vjust = 0.5), col = "white")+
  geom_label_repel(aes(label = Number_of_usage), col = "white",
                   size = 3.5, nudge_x = 1.5, show.legend = FALSE)

