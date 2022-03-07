# Import Packages #
library(tidyverse)
library(RCurl)
library(skimr)
library(lubridate)


# Importing The File #
dailydata <- read_csv("dailyActivity_merged.csv")

View(dailydata)
head(dailydata)
tail(dailydata)

#Data Manipulating#
##Overview##
dailydata$ActivityDate <- mdy(dailydata$ActivityDate)
dailydata$Id <- as.character(dailydata$Id)

glimpse(dailydata)

###Creating Subset Data Part 1###
PerDay <- dailydata %>% count(ActivityDate, sort = T)
  
names(PerDay)[2] <- 'User_perday'

PerDay <- dailydata %>% group_by(ActivityDate) %>%
  summarise(total_dailySteps = sum(TotalSteps)
            ,total_dailydistance = sum(TotalDistance)
            ,total_dailycalories = sum(Calories))

PerDay

PerUser <- dailydata %>% group_by(Id) %>%
  summarise(UserSteps = sum(TotalSteps)
            ,UserDistance = sum(TotalDistance)
            ,UserCalories = sum(Calories))

PerUser

#Plotting Part 1#

ggplot(data = dailydata, aes(ActivityDate, TotalSteps, color = Id)) + 
  geom_line(size = 1) + theme(legend.position = "none") + facet_wrap(~Id)
  

ggplot(data = dailydata, aes(ActivityDate, Calories, color = Id)) + 
  geom_line(size = 1) + theme(legend.position = "none") + facet_wrap(~ Id)

ggplot(data = dailydata, aes(ActivityDate, TotalDistance, color = Id)) + 
  geom_line(size = 1) + theme(legend.position = "none") + facet_wrap(~ Id)

plot(PerDay)
plot(PerUser)

##Creating Subset Data Part2##

MinutesUsage <- dailydata %>% select(Id, ActivityDate, VeryActiveMinutes,
                                     FairlyActiveMinutes, LightlyActiveMinutes)
head(MinutesUsage)

#Plotting Part 2#

ggplot(MinutesUsage, aes(ActivityDate, VeryActiveMinutes, color = Id)) +
  geom_line(size = 1) + facet_wrap(~Id)

ggplot(MinutesUsage, aes(ActivityDate, FairlyActiveMinutes, color = Id)) +
  geom_line(size = 1) + facet_wrap(~Id)

ggplot(MinutesUsage, aes(ActivityDate, LightlyActiveMinutes, color = Id)) +
  geom_line(size = 1) + facet_wrap(~Id)


##Creating Subset Data Part 3##
UsersUsage <- MinutesUsage %>% group_by(ActivityDate) %>%
  summarise(VeryActive = sum(VeryActiveMinutes),FairlyActive = sum(FairlyActiveMinutes)
            ,LightlyActive = sum(LightlyActiveMinutes))

head(UsersUsage)

DailyUsage <- MinutesUsage %>% group_by(Id) %>% 
  summarise(VeryActive = sum(VeryActiveMinutes),FairlyActive = sum(FairlyActiveMinutes)
            ,LightlyActive = sum(LightlyActiveMinutes))

head(DailyUsage)

#Saving the Subset Data#

write.csv(dailydata, file = "FitbitMergedData.csv")
write.csv(PerDay, file = "Fitbitsubset1.csv")
write.csv(PerUser, file = "Fitbitsubset2.csv")
write.csv(MinutesUsage, file = "Fitbitsubset3.csv")
write.csv(UsersUsage, file = "Fitbitsubset4.csv")
write.csv(DailyUsage, file = "Ftibitsubset5.csv")