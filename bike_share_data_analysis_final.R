### Divvy_Exercise_Full_Year_Analysis ###

# This analysis is based on the Divvy case study "'Sophisticated, Clear, and Polished’: Divvy and Data Visualization" written by Kevin Hartman (found here: https://artscience.blog/home/divvy-dataviz-case-study). The purpose of this script is to consolidate downloaded Divvy data into a single dataframe and then conduct simple analysis to help answer the key question: “In what ways do members and casual riders use Divvy bikes differently?”

# # # # # # # # # # # # # # # # # # # # # # # 
# Install required packages
# tidyverse for data import and wrangling
# lubridate for date functions 
# ggplot for visualization
# # # # # # # # # # # # # # # # # # # # # # #  
install.packages("tidyverse")
install.packages("lubridate")
install.packages("ggplot2")


library(tidyverse)  #helps wrangle data
library(lubridate)  #helps wrangle date attributes
library(ggplot2)  #helps visualize data
getwd() #displays your working directory
#setwd


# STEP 1: COLLECT DATA

# Upload Divvy datasets (csv files) here
may_2022 <- read_csv("202205-divvy-tripdata.csv")
jun_2022 <- read_csv("202206-divvy-tripdata.csv")
jul_2022 <- read_csv("202207-divvy-tripdata.csv")
aug_2022 <- read_csv("202208-divvy-tripdata.csv")
sep_2022 <- read_csv("202209-divvy-tripdata.csv")
oct_2022 <- read_csv("202210-divvy-tripdata.csv")
nov_2022 <- read_csv("202211-divvy-tripdata.csv")
dec_2022 <- read_csv("202212-divvy-tripdata.csv")
jan_2023 <- read_csv("202301-divvy-tripdata.csv")
feb_2023 <- read_csv("202302-divvy-tripdata.csv")
mar_2023 <- read_csv("202303-divvy-tripdata.csv")
apr_2023 <- read_csv("202304-divvy-tripdata.csv")


# STEP 2: WRANGLE DATA AND COMBINE INTO A SINGLE FILE

# Compare column names each of the files
# While the names don't have to be in the same order, they DO need to match perfectly before we can use a command to join them into one file

colnames(may_2022)
colnames(jun_2022)
colnames(jul_2022)
colnames(aug_2022)
colnames(sep_2022)
colnames(oct_2022)
colnames(nov_2022)
colnames(dec_2022)
colnames(jan_2023)
colnames(feb_2023)
colnames(mar_2023)
colnames(apr_2023)

# Inspect the dataframes and look for incongruencies

months <- c(may_2022,jun_2022,jul_2022,aug_2022,sep_2022,oct_2022,nov_2022,dec_2022,jan_2023,feb_2023,mar_2023,apr_2023)

str(may_2022)
str(apr_2023)
#Datatypes for columns are consistent across all the months


# Stack individual month's data frames into one big data frame
all_trips <- bind_rows(may_2022,jun_2022,jul_2022,aug_2022,sep_2022,oct_2022,nov_2022,dec_2022,jan_2023,feb_2023,mar_2023,apr_2023)
colnames(all_trips)
# Remove lat, long, birthyear, and gender fields as this data was is not needed for any of the analysis and simplifying the dataset will make it more manageable.
all_trips <- all_trips %>%  
  select(-c(start_lat, start_lng, end_lat, end_lng))


#======================================================
# STEP 3: CLEAN UP AND ADD DATA TO PREPARE FOR ANALYSIS
#======================================================
# Inspect the new table that has been created
colnames(all_trips)  #List of column names
nrow(all_trips)  #How many rows are in data frame?
dim(all_trips)  #Dimensions of the data frame?
head(all_trips)  #See the first 6 rows of data frame.  Also tail(all_trips)
str(all_trips)  #See list of columns and data types (numeric, character, etc)
summary(all_trips)  #Statistical summary of data. Mainly for numerics

# There are a few problems we will need to fix:
# (2) The data can only be aggregated at the ride-level, which is too granular. We will want to add some additional columns of data -- such as day, month, year -- that provide additional opportunities to aggregate the data.
# (3) We will want to add a calculated field for length of ride.
# (4) There are some rides where tripduration shows up as negative, including several hundred rides where Divvy took bikes out of circulation for Quality Control reasons. We will want to delete these rides.


# Check to make sure the proper number of observations were reassigned
table(all_trips$member_casual)

# Add columns that list the date, month, day, and year of each ride
# This will allow us to aggregate ride data for each month, day, or year ... before completing these operations we could only aggregate at the ride level
# https://www.statmethods.net/input/dates.html more on date formats in R found at that link
all_trips$date <- as.Date(all_trips$started_at, format = "%m/%d/%Y %H:%M")
all_trips$month <- format(as.Date(all_trips$date), "%m")
all_trips$day <- format(as.Date(all_trips$date), "%d")
all_trips$year <- format(as.Date(all_trips$date), "%Y")
all_trips$day_of_week <- format(as.Date(all_trips$date), "%A")

# Add a "ride_length" calculation to all_trips (in seconds)
# https://stat.ethz.ch/R-manual/R-devel/library/base/html/difftime.html
date_format <- "%m/%d/%y %H:%M"
all_trips$started_at <- as.POSIXct(all_trips$started_at, format = date_format)
all_trips$ended_at <- as.POSIXct(all_trips$ended_at, format = date_format)

all_trips$ride_length <- difftime(all_trips$ended_at,all_trips$started_at)

# Inspect the structure of the columns

str(all_trips)
View(all_trips)
print(all_trips$ride_length)
print(all_trips$started_at)
head(all_trips)
# Convert "ride_length" from Factor to numeric so we can run calculations on the data
class(all_trips$ride_length)
all_trips$ride_length <- as.numeric(all_trips$ride_length)
is.numeric(all_trips$ride_length)

# Remove "bad" data
# The dataframe includes a few hundred entries when bikes were taken out of docks and checked for quality by Divvy or ride_length was negative
# We will create a new version of the dataframe (v2) since data is being removed
# https://www.datasciencemadesimple.com/delete-or-drop-rows-in-r-with-conditions-2/
all_trips_v2 <- all_trips[!(all_trips$start_station_name == "HQ QR" | all_trips$ride_length<0),]
View(all_trips_v2)


# STEP 4: CONDUCT DESCRIPTIVE ANALYSIS

# Descriptive analysis on ride_length (all figures in seconds)

all_trips_v2 <- all_trips_v2[complete.cases(all_trips_v2$ride_length), ] ## I used this to remove NA ride length values from the dataset (along with the rest of their rows)


mean(all_trips_v2$ride_length) #straight average (total ride length / rides)
median(all_trips_v2$ride_length) #midpoint number in the ascending array of ride lengths
max(all_trips_v2$ride_length) #longest ride
min(all_trips_v2$ride_length) #shortest ride
# You can condense the four lines above to one line using summary() on the specific attribute
summary(all_trips_v2$ride_length)

# Compare members and casual users
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = mean)
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = median)
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = max)
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = min)

# See the average ride time by each day for members vs casual users
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual + all_trips_v2$day_of_week, FUN = mean)

# Notice that the days of the week are out of order. Let's fix that.
all_trips_v2$day_of_week <- ordered(all_trips_v2$day_of_week, levels=c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))

# Now, let's run the average ride time by each day for members vs casual users
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual + all_trips_v2$day_of_week, FUN = mean)

# checking colnames again
colnames(all_trips_v2)

# analyze ridership data by type and weekday
all_trips_v2 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>%  #creates weekday field using wday()
  group_by(member_casual, weekday) %>%  #groups by usertype and weekday
  summarise(number_of_rides = n()							#calculates the number of rides and average duration 
            ,average_duration = mean(ride_length)) %>% 		# calculates the average duration
  arrange(member_casual, weekday)								# sorts

# Let's visualize overall member_casual differences in ride count
all_trips_v2 %>%
  group_by(member_casual) %>%
  summarise(number_of_rides = n()) %>%
  ggplot(aes(x=member_casual, y=number_of_rides, fill = member_casual)) +
  geom_col(position = "dodge") + 
  theme(legend.position = "none") +
  labs(x= "Membership Type", y = "Number of Rides", title = "Total Ride Count by
Membership Type") +
  scale_x_discrete(label= c("Casual", "Annual"))
  
# Now for ride duration
all_trips_v2 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>%
  group_by(member_casual, weekday) %>%
  summarise(number_of_rides = n()
            ,average_duration = mean(ride_length)) %>%
  arrange(member_casual, weekday) %>%
  ggplot(aes(x=member_casual, y=average_duration, fill = member_casual)) +
  geom_col(position = "dodge") +
  theme(legend.position = "none") +
  labs(x= "Membership Type", y = "Average Ride Duration", title = "Average Ride Duration by
Membership Type") +
  scale_x_discrete(label= c("Casual", "Annual"))


# Let's visualize the number of rides by rider type
  all_trips_v2 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>% 
  group_by(member_casual, weekday) %>% 
  summarise(number_of_rides = n()) %>%
  arrange(member_casual, weekday)  %>% 
  ggplot(aes(x = weekday, y = number_of_rides, fill = member_casual)) +
  geom_col(position = "dodge") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  scale_fill_discrete(name = "Membership Type", labels = c("Casual Member", "Annual Member")) +
  labs(x= "Day of Week", y= "Number of Rides", title = "Ride Count per Day by 
       Customer Type")

# Let's create a visualization for average duration (converted duration to minutes because I think it's easier to digest)
all_trips_v2 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>% 
  group_by(member_casual, weekday) %>% 
  summarise(number_of_rides = n()
            ,average_duration = mean(ride_length)) %>%
  arrange(member_casual, weekday)  %>% 
  ggplot(aes(x = weekday, y = average_duration/60, fill = member_casual)) + 
  geom_col(position = "dodge") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  scale_fill_discrete(name = "Membership Type", labels = c("Casual Member", "Annual Member")) +
  labs(x= "Day of Week", y= "Average Ride Duration
  (minutes)", title = "Ride Duration per Day by 
       Customer Type")

#=================================================
# STEP 5: EXPORT SUMMARY FILE FOR FURTHER ANALYSIS
#=================================================
# Create a csv file that we will visualize in Excel, Tableau, or my presentation software
# N.B.: This file location is for a Mac. If you are working on a PC, change the file location accordingly (most likely "C:\Users\YOUR_USERNAME\Desktop\...") to export the data. You can read more here: https://datatofish.com/export-dataframe-to-csv-in-r/
counts <- aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual + all_trips_v2$day_of_week, FUN = mean)
write.csv(counts, file = 'avg_ride_length.csv')

#You're done! Congratulations!

