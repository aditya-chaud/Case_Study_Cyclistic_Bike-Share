#Importing Libraries
library(tidyverse)
library(ggplot2)
library(lubridate)

#Importing datasets
df_q2_2019 <- read_csv("C:/../video/dddd/Data Analytics/course 8 capstone/datasets/last 12 months of datasets/Divvy_Trips_2019_Q2.csv")
df_q3_2019 <- read_csv("C:/../video/dddd/Data Analytics/course 8 capstone/datasets/last 12 months of datasets/Divvy_Trips_2019_Q3.csv")
df_q4_2019 <- read_csv("C:/../video/dddd/Data Analytics/course 8 capstone/datasets/last 12 months of datasets/Divvy_Trips_2019_Q4.csv")
df_q1_2020 <- read_csv("C:/../video/dddd/Data Analytics/course 8 capstone/datasets/last 12 months of datasets/Divvy_Trips_2020_Q1.csv")

  
#Renaming columns to make consistent column names in all datasets so that we can merge later  
#in df_q2_2019
df_q2_2019 <- df_q2_2019 %>% 
  rename('Ride_id'='01 - Rental Details Rental ID',
         'Start_Time'='01 - Rental Details Local Start Time',
         'End_Time'='01 - Rental Details Local End Time',
         'Bike_id'='01 - Rental Details Bike ID',
         'Trip_Duration_secs'='01 - Rental Details Duration In Seconds Uncapped',
         'From_Station_id'='03 - Rental Start Station ID',
         'From_Station_Name'='03 - Rental Start Station Name',
         'To_Station_id'='02 - Rental End Station ID',
         'To_Station_Name'='02 - Rental End Station Name',
         'User_type'='User Type',
         'Gender'='Member Gender',
         'Birth_Year'='05 - Member Details Member Birthday Year'
  )

#in df_q3_2019 
df_q3_2019 <- df_q3_2019 %>% 
  rename('Ride_id'='trip_id',
         'Start_Time'='start_time',
         'End_Time'='end_time',
         'Bike_id'='bikeid',
         'Trip_Duration_secs'='tripduration',
         'From_Station_id'='from_station_id',
         'From_Station_Name'='from_station_name',
         'To_Station_id'='to_station_id',
         'To_Station_Name'='to_station_name',
         'User_type'='usertype',
         'Gender'='gender',
         'Birth_Year'='birthyear'
  )

#in_df_q4_2019
df_q4_2019 <- df_q4_2019 %>% 
  rename('Ride_id'='trip_id',
         'Start_Time'='start_time',
         'End_Time'='end_time',
         'Bike_id'='bikeid',
         'Trip_Duration_secs'='tripduration',
         'From_Station_id'='from_station_id',
         'From_Station_Name'='from_station_name',
         'To_Station_id'='to_station_id',
         'To_Station_Name'='to_station_name',
         'User_type'='usertype',
         'Gender'='gender',
         'Birth_Year'='birthyear'
  )

#in df_q1_2020
df_q1_2020 <- df_q1_2020 %>% 
  rename('Ride_id'='ride_id',
         'Start_Time'='started_at',
         'End_Time'='ended_at',
         'Bike_id'='rideable_type',
         'From_Station_id'='start_station_id',
         'From_Station_Name'='start_station_name',
         'To_Station_id'='end_station_id',
         'To_Station_Name'='end_station_name',
         'User_type'='member_casual'
  )

#now we need to combine all the dataframe into the big dataframe
#but first we need to check the datatypes of all the columns in each dataframe to make them consistent in each dataframe

#using the following function for quick analysis
glimpse(df_q2_2019)
glimpse(df_q3_2019)
glimpse(df_q4_2019)
glimpse(df_q1_2020)

#since there is not Bike_id column in df_q1_2020 instead it has 'rideable_type' which has three types of bikes.
#so we need to change the datatype of Bike_id in df_q2_2019 to character(chr). As changing Character to number is not possible
df_q2_2019$Bike_id <- as.character(df_q2_2019$Bike_id)
# now check if Bike_id is character
glimpse(df_q2_2019)


#now do similar for df_q3_2019 and df_q4_2019
df_q3_2019$Bike_id <- as.character(df_q3_2019$Bike_id)
df_q4_2019$Bike_id <- as.character(df_q4_2019$Bike_id)

#Similarly, we need to change the datatype of Ride_id in df_q2_2019,df_q3_2019,df_q4_2019  too 
#because it's datatype is character in df_q1_2020 and its not possible to change to numeric since it has 
#alphabets too in df_q1_2020
df_q2_2019$Ride_id <- as.character(df_q2_2019$Ride_id)
df_q3_2019$Ride_id <- as.character(df_q3_2019$Ride_id)
df_q4_2019$Ride_id <- as.character(df_q4_2019$Ride_id)


#Also User_Type in df_q1_2020 has different two options member and casual which refers to 
#member as subscriber and casual as customer. so changing subscriber and customer to member and casual
#in each of the other dataframes. since it is defined casual as non-subscriber and member as those who have 1 year subscription.
#in the business problem.

df_q2_2019 <- df_q2_2019 %>%
  mutate(User_type = case_when(
    User_type == "Customer" ~ "casual",
    User_type == "Subscriber" ~ "member",
    TRUE ~ User_type
  ))
#check the new usertypes
print(unique(df_q2_2019$User_type))

#doing this in other dataframes too
df_q3_2019 <- df_q3_2019 %>% 
  mutate(User_type = case_when(
    User_type == "Customer" ~ "casual",
    User_type == "Subscriber" ~  "member",
    TRUE ~ User_type
  ))

df_q4_2019 <- df_q4_2019 %>% 
  mutate(User_type = case_when(
    User_type == "Customer" ~ "casual",
    User_type == "Subscriber" ~ "member",
    TRUE ~ User_type
  ))

#Cleaning df_q2_2019
#we need to make equal number of columns to merge them. So we delete some columns that are not needed for our analysis
#example: Gender column in df_q2_2019 has 169469 Null values. which make about 16% of total data 
#ie. total rows:1048575, null_cells_inGender=169469: % of null rows=(169469/1048575)*100% ie.16%
#Gender is mostly specified for member riders only not casual riders. so we cant fill any values since there is not any pattern
num_of_null_rows_in_df_q2_2019 <- nrow(df_q2_2019[is.na(df_q2_2019$Gender),])
print(num_of_null_rows_in_df_q2_2019)
#removing 16% of data in just a dataframe is not good we could lose other important data.

#also Checking null values in Birth_Year column
num_of_null_rows_in_df_q2_2019_birth_year <- nrow(df_q2_2019[is.na(df_q2_2019$Birth_Year),])
print(num_of_null_rows_in_df_q2_2019_birth_year)
#165108 is 15.7%. so removing this amount of data will result in loss of other important data
#Also, removing the rows null values in both Gender and Birth_Year will result in more than 16% of rows.

#so we remove both the column 
df_q2_2019 <- df_q2_2019 %>% 
  select(-Gender, -Birth_Year)

#we also remove the column Trip_Duration_secs 
#Because we will add Ride_Length column later for analysing the amount of time the bikes are rented and in df_q1_2020 too.
df_q2_2019$Trip_Duration_secs <- NULL

#Cleaning df_q3_2019
null_rows_df_q3_2019_Gender <- nrow(df_q3_2019[is.na(df_q3_2019$Gender),])
print(null_rows_df_q3_2019_Gender)
#total null rows are 195055 which is 18% of total data.

null_rows_df_q3_2019_User_type <- nrow(df_q3_2019[is.na(df_q3_2019$Birth_Year),])
print(null_rows_df_q3_2019_User_type)
#total null rows  189267 which is  18% too.
#we also need to remove Trip_Duration_secs column for same reason as of df_q2_2019
df_q3_2019 <- df_q3_2019 %>% 
  select(-Gender, -Birth_Year, -Trip_Duration_secs)
# glimpse(df_q3_2019)

#cleaning df_q4_2019
null_rows_df_q4_2019_Gender <- nrow(df_q4_2019[is.na(df_q4_2019$Gender),])
null_rows_df_q4_2019_Birth_Year <- nrow(df_q4_2019[is.na(df_q4_2019$Birth_Year),])
print(null_rows_df_q4_2019_Gender)
print(null_rows_df_q4_2019_Birth_Year)

#removing Gender, Birth_Year and Trip_Duration_secs columns
df_q4_2019 <- df_q4_2019 %>% 
  select(-Gender, -Birth_Year, -Trip_Duration_secs)
# glimpse(df_q4_2019)

#Cleaning df_q1_2020 
#removing some columns that is not needed for analysis
df_q1_2020 <- df_q1_2020 %>% 
  select(-start_lat, -start_lng, -end_lat, -end_lng)

#glimpse(df_q1_2020)

# Now the data is ready to be merged into a big dataframe. Lets say new dataframe be df
df <- rbind(df_q2_2019, df_q3_2019, df_q4_2019,df_q1_2020)

##Preparing the merged dataframe for analysis
#checking if there is any na values
null_df <- df[rowSums(is.na(df)) > 0, ]

#So we need to remove such rows
df <- na.omit(df)
# View(df)

#Adding the weekday column which means the day in which the bike has been rented
#Adding the month column by extracting the value of start_time column 
#to know in which day or month most rides are rented.

glimpse(df)

#change the datatype of Start_Time and End_Time from character to POSIXct format ie date time format
#It will help to extract weekday and month
df$Start_Time <- as.POSIXct(df$Start_Time, format = "%m/%d/%Y %H:%M")
df$End_Time <- as.POSIXct(df$End_Time, format = "%m/%d/%Y %H:%M")

#Now adding Ride_Length column that gives total time the bike had been rented
df$Ride_Length <-df$End_Time - df$Start_Time

#checking na rows again
null_df <- df[rowSums(is.na(df)) > 0, ]

#now adding column Weekday and Month by extracting each weeekday and month from Start_Time column
df$Weekday <- weekdays(df$Start_Time)
#we can use capital B to get full January, February words instead of jan, feb or month(df$Start_Time) for number of month
df$Month <- format(df$Start_Time, "%b")

glimpse(df)

#Checking the rides where Ride_Length is -ve because of starting time being lesser than the ending time. And removing 
#the rides lesser than 10 seconds because renting for only 10 seconds does not make sense.
#And there are some rows where Ride_Length is 0 sec whoch also does not make sense.
short_rides <- df[df$Ride_Length < 10, , drop = FALSE]
View(short_rides)

#Removing these short rides. 6096 rows
df <- df[df$Ride_Length >= 10, , drop = FALSE]

#Analysing the data(df)
#Now the data is ready for analyse analysing.
#Now we see statistical analysis about the data in our cleaned dataframe.

#Here is the brief description about the column in our dataframe
# Ride_id: Unique ID for each bike ride or trip.
# Start_Time: Time when the ride begins.
# End_Time: Time when the bike is returned.
# Bike_id: ID of the bike used.
# From_Station_id: ID of the starting station.
# From_Station_Name: Name of the starting station.
# To_Station_id: ID of the station where the bike is returned.
# To_Station_Name: Name of the station where the bike is returned.
# Ride_Length: Duration of the bike rental.
# User_type: Types of users: "Casual riders" buy single-ride or full-day passes, "Cyclistic members" have annual memberships.
# Weekday: Day of the week
# Month: Month of the year

# Major statistical analysis we will be doing
#1. Count the number of casual and member riders(frequency)
#2. Calculate how much each type of riders spend renting the bikes
#3. Overall min_ride_length and max_ride_length
#4. Min Ride_Length and Max Ride_Length for both the riders
#to get the summary for Ride_Length change it to numeric
#5. Calculate the mean and median Ride_length(trip duration) for both user types
#6.See the ride_lengths and number of rides in weekday
#7. see the ride_length and number of rides in months
#8.Looking the top 10 most popular starting station for both the user types
#9. Looking the top 10 most popular ending station for both the user types
#10. Looking the top 10 popular starting station for casual user type
#11. Looking top 10 popular ending station for casual user type

head(df,n=5)


#1. Total number of rides for each usertype
user_type_ridecounts <- df %>%
  group_by(User_type) %>%
  summarize(count = n())

View(user_type_ridecounts)


#2. Calculate how much each type of riders spend renting the bikes
trip_duration <- df %>% 
  group_by(User_type) %>% 
  summarize(total_time = sum(Ride_Length))
  
View(trip_duration)


#3. Overall min_ride_length and max_ride_length
#changing the Ride_Length to numeric 
df$Ride_Length <- as.numeric(df$Ride_Length)
max(df$Ride_Length)
min(df$Ride_Length)

#4. Min Ride_Length and Max Ride_Length for each of the riders
min_max_ride_length <- df %>%
  group_by(User_type) %>%
  summarize(max_ride_length = max(Ride_Length), min_ride_length = min(Ride_Length))

View(min_max_ride_length)

#5. Calculate the mean and median Ride_length(trip duration) for both user types
mean_median_ride_length <- df %>%
  group_by(User_type) %>%
  summarize(mean_ride_length = mean(Ride_Length), median_ride_length = median(Ride_Length))

#6.See the ride_lengths and number of rides ie no of rents of cycles in each day of weekday
weekly_rides_summary <- df %>%
  group_by(Weekday, User_type) %>%
  summarize(Total_Ride_Length = sum(Ride_Length), total_rides = n() )
#arranging the weekdays in orders 
weekly_rides_summary$Weekday <- factor(weekly_rides_summary$Weekday, levels = c('Sunday', 'Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday', 'Saturday'))

#7. see the ride_length and number of rides in months
monthly_rides_summary <- df %>%
  group_by(User_type, Month) %>%
  summarize(Total_Ride_Length = sum(Ride_Length), total_rides = n())
monthly_rides_summary$Month <- factor(monthly_rides_summary$Month, levels = c('Sunday', 'Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun','Jul','Aug','Sep','Oct','Nov','Dec'))

#8. Looking the top 10 most popular starting station for both the user types
Starting_Station_counts <- df %>%
  group_by(From_Station_Name) %>%
  summarise(frequency = n()) %>%
  arrange(desc(frequency)) %>% 
  top_n(10, wt = frequency) %>% 
  ungroup()

#9. Looking the top 10 popular starting station for casual user type
Starting_Station_casual <- df %>%
  filter(User_type == "casual") %>%
  group_by(From_Station_Name) %>%
  summarise(frequency = n()) %>%
  arrange(desc(frequency)) %>% 
  top_n(10, wt=frequency) %>% 
  ungroup()

#10. Looking the top 10 most popular ending station for both the user types
Ending_station_counts <- df %>% 
  group_by(To_Station_Name) %>% 
  summarise(frequency =n()) %>% 
  arrange(desc(frequency)) %>% 
  top_n(10, wt = frequency) %>% 
  ungroup()

#11. Looking top 10 popular ending station for casual user type
Ending_Station_casual <- df %>%
  filter(User_type == "casual") %>%
  group_by(To_Station_Name) %>%
  summarise(frequency = n()) %>%
  arrange(desc(frequency)) %>% 
  top_n(10, wt=frequency) %>% 
  ungroup()

#total starting stations 
length(unique(df$From_Station_Name))
length(unique(df$To_Station_Name))




##Visualizations
#Visualizations are made from reference to analyse part.
#1. Total number of rides for each usertype
ggplot(data=user_type_ridecounts, aes(x = User_type, y = count, fill = User_type)) +
  geom_bar(stat = "identity") + labs(title = 'Number of rides counts for each user type', x = 'User type', y = 'Number of rides')



#2. Calculate how much each type of riders spend time renting the bikes
ggplot(data=trip_duration, aes(x = User_type, y = total_time, fill = User_type)) +
  geom_bar(stat = "identity") +labs(title = 'Total time each types of riders spend renting the cycles', x = 'User type', y = 'total ride lengths(seconds)')


#6.1 See the ride_lengths in each day of weekday
ggplot(weekday_rides_summary, aes(x = Weekday, y = Total_Ride_Length, fill = User_type)) +
  geom_bar(stat = 'identity', position = position_dodge(), width = 0.7) +
  labs(title = 'Ride length by Weekday and User Type', x = 'Weekday', y = 'Ride Length (secs)') +
  scale_fill_discrete(name = 'User Type')

#6.2 See the number of rides ie no of rents of cycles in each day of weekdays
ggplot(weekday_totalrides, aes(x = Weekday, y = total_rides, fill = User_type)) +
  geom_bar(stat = 'identity', position = position_dodge(), width = 0.7) +
  labs(title = 'Total no of rides by Weekday and User Type', x = 'Weekday', y = 'No of rides') +
  scale_fill_discrete(name = 'User Type')

#7.1 line plot showing the Ride Length by Month and User Type
ggplot(monthly_rides_summary, aes(x = Month, y = Total_Ride_Length, group = User_type, color = User_type)) +
  geom_line() +
  geom_point() +
  labs(title = "Ride Length by Month and User Type" , x = "Month", y = "Ride Length (secs)") +
  scale_color_manual(values = c("casual" = "blue", "member" = "red")) +
  theme_minimal()

#7.2 line plot showing the number of rides rented each month by each user type
ggplot(monthly_rides_summary, aes(x = Month, y = total_rides, group = User_type, color = User_type)) +
  geom_line() +
  geom_point() +
  labs(title = "Number of Rides rented each Month and User Type" , x = "Month", y = "Number of rides",) +
  scale_color_manual(values = c("casual" = "blue", "member" = "red")) +
  theme_minimal()

#8. Looking the top 10 most popular starting station for both the user types
ggplot(Starting_Station_counts, aes(y = reorder(From_Station_Name, frequency), x = frequency)) +
  geom_bar(stat = "identity", fill = "dodgerblue") +
  labs(title = "Top 10 most popular starting stations", x = "Frequency", y = "Station Name")

#9. Looking the top 10 popular starting station for casual user type
ggplot(Starting_Station_casual, aes(y = reorder(From_Station_Name, frequency), x = frequency)) +
  geom_bar(stat = "identity", fill = "dodgerblue") +
  labs(title = "Top 10 most popular starting station for casual riders", x = "Frequency", y = "Station Name")

#10. Top 10 most popular ending station for both the user types
ggplot(Ending_station_counts, aes(y = reorder(To_Station_Name, frequency), x = frequency)) +
  geom_bar(stat = "identity", fill = "orange") +
  labs(title = "Top 10 most popular ending station", x = "Frequency", y = "Station Name")


#11. Looking top 10 popular ending station for casual user type
ggplot(Ending_Station_casual, aes(y = reorder(To_Station_Name, frequency), x = frequency)) +
  geom_bar(stat = "identity", fill = "orange") +
  labs(title = "Top 10 most popular ending station for casual riders", x = "Frequency", y = "Station Name")

