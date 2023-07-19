# Load available data sets (18)
activity <- read.csv("../project/dailyActivity_merged.csv")
calories <- read.csv("../project/dailyCalories_merged.csv")
intensities <- read.csv("../project/dailyIntensities_merged.csv")
steps <- read.csv("../project/dailySteps_merged.csv")
heart <- read.csv("../project/heartrate_seconds_merged.csv")
h_calories <- read.csv("../project/hourlyCalories_merged.csv")
h_intensities <- read.csv("../project/hourlyIntensities_merged.csv")
h_steps <- read.csv("../project/hourlySteps_merged.csv")
m_calories_n <- read.csv("../project/minuteCaloriesNarrow_merged.csv")
m_calories_w <- read.csv("../project/minuteCaloriesWide_merged.csv")
m_intensities_n <- read.csv("../project/minuteIntensitiesNarrow_merged.csv")
m_intensities_w <- read.csv("../project/minuteIntensitiesWide_merged.csv")
met <- read.csv("../project/minuteMETsNarrow_merged.csv")
sleep_m <- read.csv("../project/minuteSleep_merged.csv")
m_steps_n <- read.csv("../project/minuteStepsNarrow_merged.csv")
m_steps_w <- read.csv("../project/minuteStepsWide_merged.csv")
sleep_d <- read.csv("../project/sleepDay_merged.csv")
weight <- read.csv("../project/weightLogInfo_merged.csv")

# Install needed packages.
install.packages("here")
library("here")
install.packages("skimr")
library("skimr")
install.packages("janitor")
library("janitor")
install.packages("dplyr")
library("dplyr")
install.packages("tidyr")
library("tidyr")
install.packages("lubridate")
library("lubridate")
install.packages("ggplot2")
library("ggplot2")

# Check the amount of records for all data sets (by Id)
n_distinct(activity$Id) # 33

n_distinct(calories$Id) # 33
n_distinct(intensities$Id) # 33
n_distinct(steps$Id) # 33
n_distinct(met$Id) # 33
n_distinct(h_calories$Id) # 33
n_distinct(h_intensities$Id) # 33
n_distinct(h_steps$Id) # 33
n_distinct(m_calories_n$Id) # 33
n_distinct(m_calories_w$Id) # 33
n_distinct(m_intensities_n$Id) # 33
n_distinct(m_intensities_w$Id) # 33
n_distinct(m_steps_n$Id) # 33
n_distinct(m_steps_w$Id) # 33
n_distinct(sleep_m$Id) # 24
n_distinct(sleep_d$Id) # 24
n_distinct(weight$Id) # 8
n_distinct(heart$Id) # 14


# Use only data sets with users amount above 30 (information from the project).

# Organizing and cleaning data sets that will be used for analyses.
# Start with data set 'activity' which contains all available users data about:
# steps, distance, active minutes and calories.
skim_without_charts(activity)
head(activity)
glimpse(activity)
summary(activity)

# Clean data set 
# Data set contains 15 columns
# Data set contains data from 33 users collected by period from 12/04/2016 to 12/05/2016.

# Column 'ActivityDate' is in a wrong format, change for date format
clean_activity <- mutate(activity, ActivityDate=as.Date(ActivityDate, format = "%m/%d/%Y"))
glimpse(clean_activity)

#Check columns
clean_names(clean_activity)

# Check for missing values
# Find location of missing values
print("Position of missing values -")
which(is.na(clean_activity))

# Count total missing values 
print("Count of total missing values - ")
sum(is.na(clean_activity)) # 0

# Check for duplicated rows
sum(duplicated(clean_activity))
View(clean_activity)
head(clean_activity)
glimpse(clean_activity)

# Add day of the week to data set
clean_activity$WeekDay <- wday(clean_activity$ActivityDate, label=TRUE, week_start = 1)

View(clean_activity)
glimpse(clean_activity)

# Group data by week day to check which day is the most active - by steps.
week_day_activity <- aggregate(clean_activity$TotalSteps, by=list(WeekDay=clean_activity$WeekDay), FUN=sum)
colnames(week_day_activity)[2] ="TotalSteps"
week_day_activity

# Plot the data to see activity.
ggplot(data=week_day_activity, aes(WeekDay, TotalSteps, fill=TotalSteps))+
  geom_bar(position = "dodge", stat = "identity")+
  labs(title="Total amount of steps per Week Day")
 
# Tuesday - the most active day according to the amount of steps.
# Sunday - the least active day according to the amount of steps.

# Check calories burn by week day.
week_day_calories <- aggregate(clean_activity$Calories, by=list(WeekDay=clean_activity$WeekDay), FUN=sum)
colnames(week_day_calories)[2] ="Calories"
week_day_calories
ggplot(data=week_day_calories, aes(WeekDay, Calories, fill=Calories))+
  geom_bar(position = "dodge", stat = "identity")+
  labs(title="Total calories per week day")

# Tuesday is the day with the most burned calories, and Sunday with the least.

# It seems that the Tuesday is the most active day in the week. 
# Key take would be to focus on activating users in other day of the week. 

# We've got 4 groups of active distance data:
  # Very Active
  # Moderate
  # Light
  # Sedentary

# Count mean or every group 
mean(clean_activity$VeryActiveMinutes)
mean(clean_activity$FairlyActiveMinutes)
mean(clean_activity$LightlyActiveMinutes)
mean(clean_activity$SedentaryMinutes)

# Sum activity according to the day of week to see how different level of activity are spread during the week.

# Plot 'VeryActiveMinutes' with week days.
# Group data by week day.

sum_activity <- clean_activity[,c("WeekDay", "VeryActiveMinutes", "FairlyActiveMinutes", "LightlyActiveMinutes", "SedentaryMinutes")]
sum_activity_2 <- sum_activity %>% 
  group_by(WeekDay) %>% 
  summarise(VeryActiveMinutesTotal=sum(VeryActiveMinutes),
            FairlyActiveMinutesTotal=sum(FairlyActiveMinutes),
            LightlyActiveMinutesTotal=sum(LightlyActiveMinutes),
            SedentaryMinutesTotal=sum(SedentaryMinutes))
View(sum_activity_2)

sum_activity_2_total <- sum_activity_2 %>% group_by(WeekDay) %>% 
  summarise(Total=sum(VeryActiveMinutesTotal + FairlyActiveMinutesTotal + LightlyActiveMinutesTotal))
View(sum_activity_2_total)

# Check mean value for total activity and sedentary.
mean(sum_activity_2_total$Total)
mean(sum_activity_2$SedentaryMinutesTotal)
# Total time - 1,219.2949 recorded.



# To plot data we will have to change format for long.
sum_activity_2_long <- sum_activity_2 %>% pivot_longer(cols=c( 'VeryActiveMinutesTotal', 'FairlyActiveMinutesTotal', 'LightlyActiveMinutesTotal', 'SedentaryMinutesTotal'),
                    names_to='Activity',
                    values_to='Minutes')

View(sum_activity_2_long)
# Plot summarized data for each week day.
ggplot(data=sum_activity_2_long, aes(x=WeekDay, y=Minutes, fill=Activity))+
         geom_bar(stat = "identity")

# Create new table with merged data of hourly intensities, calories and steps.
# Clean and prepare data for merging.

head(h_intensities)
head(h_calories)
head(h_steps)

# Merge above data into one table
h_activity <- merge(h_calories, h_steps, by=c('Id', 'ActivityHour'))
h_activity <- merge(h_activity, h_intensities, by=c('Id', 'ActivityHour'))
head(h_activity)
glimpse(h_activity)

# Change format of ActivityHour column
h_activity$ActivityHour <- as.POSIXct(h_activity$ActivityHour, format = "%m/%d/%Y %I:%M:%S %p")
glimpse(h_activity)
View(h_activity)


clean_names(h_activity)

which(is.na(h_activity))

sum(is.na(h_activity))

sum(duplicated(h_activity))

# Add week day column.
h_activity$WeekDay <- wday(h_activity$ActivityHour, label=TRUE, week_start = 1)
glimpse(h_activity)

# Split column with date for column data , hour. Columns change into "chr" columns again.
h_activity <- tidyr::separate(h_activity, ActivityHour, c("ActivityDate", "ActivityTime"), sep = " ")
glimpse(h_activity)




# Create new data set with grouping data and sorted data.
# To see how activity look like in each week day on evey hour (summed).
# Group data by week day and sort by hour and add calories, steps and total intensity.

h_activity_grouped <- h_activity %>% group_by(WeekDay, ActivityTime) %>% arrange(ActivityTime) %>% 
  summarise(TotalCalories=mean(Calories),
            TotalSteps=mean(StepTotal),
            TotalIntensity=mean(TotalIntensity))

View(h_activity_grouped)

# Now when data are grouped by week day and hours we can check how the activity looks like in every 
# week day by hours.

# Plot  calories data for each week day.
ggplot(h_activity_grouped, aes(x=ActivityTime, y=TotalCalories, color=WeekDay)) + geom_line() + geom_point()


# Plot steps data for each week day.
ggplot(h_activity_grouped, aes(x=ActivityTime, y=TotalSteps, color=WeekDay)) + geom_point()

# Plot intensity data fo each week day.
ggplot(h_activity_grouped, aes(x=ActivityTime, y=TotalIntensity, color=WeekDay)) + geom_point()



# -------------------------------------- below just testing


l<-h_activity_grouped[h_activity_grouped$WeekDay=="Monday"]
ggplot(l)+geom_line(aes(x=ActivityTime, y=TotalCalories))


ggplot(data = subset(h_activity_grouped, WeekDay == "Monday"),
         aes(x=ActivityTime, y=TotalCalories)) +
         geom_line()

ggplot(h_activity_grouped, aes(x=ActivityTime, y=TotalCalories, color=WeekDay)) + geom_line()




# Create subset for data for Monday.
monday <- h_activity_grouped[h_activity_grouped$WeekDay == "Monday", Calories, TotalSteps,]
glimpse(monday)
ggplot(monday, aes(x=ActivityTime,y=Calories)) + geom_line()



h_activity$ActivityDate <- as.Date(h_activity$ActivityHour)               # Add date column

h_activity$ActivityTime <- format(as.POSIXct(h_activity$ActivityHour),    # Add time column
                      format = "%I:%M:%S %p")

h_activity <- separate(data = h_activity, col = ActivityHour, into = c("ActivityDate", "ActivityHour"), sep = " ")
glimpse(h_activity)

# Try to group data by week day.
