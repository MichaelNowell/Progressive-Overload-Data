##Set WD and Load Library
getwd()
setwd("~/Progressive-Overload-Data")
library(readr)
library(tidyverse)
library(ggplot2)
library(lubridate)

##Load Data
fulldata <- read.csv("workoutlog.csv")
View(fulldata)

##Remove Notes Column because it has no data
fulldata <- select(fulldata, -(Notes))
View(fulldata)

##Change X...of Reps column to just be 'Reps' for easier naming convention
colnames(fulldata)[4] <- ('Reps')
View(fulldata)

##Transform Date Column from Character to Date format
class(fulldata$Date)
fulldata$Date <- mdy(fulldata$Date)

##Check change
class(fulldata$Date)

##Sort by Date
date_sorted <- arrange(fulldata,Date)
View(date_sorted)

##How many unique dates / number of workouts = 127
n_distinct(fulldata$Date)

##How many unique exercises = 21
n_distinct(fulldata$Exercise)

##Check for NA values = 0
sum(is.na(fulldata))

##Sets per Exercise sorted by highest to lowest and View results
sets_per_exercise <- table(fulldata$Exercise) %>% 
  sort(sets_per_exercise, decreasing = TRUE)
View(sets_per_exercise)

##Change to DF and alter Column names for better understanding
sets_per_exercse <- as.data.frame(sets_per_exercise)
colnames(sets_per_exercse) <- c('Exercise','Total Sets')
View(sets_per_exercse)

##Calculate Total Volume in lbs
total_volume <- date_sorted %>% 
  mutate(Total_Volume = Reps*Weight)
View(total_volume)

##Find how much volume per exercise per day, sort by date then desc volume
exercise_vol_by_day <- total_volume %>% 
  group_by(Date, Exercise) %>% 
  summarize(Sum_Total_Vol_LBs = sum(Total_Volume)) %>%
  arrange(Date, desc(Sum_Total_Vol_LBs))
View(exercise_vol_by_day)

##Chart 1 - Sets per Exercise
sets_per_exercse %>% 
  ggplot(mapping = aes(Exercise, `Total Sets`))+
  geom_point()+
  theme(axis.text.x = element_text(angle = 60, vjust = 1.0, hjust=1))


##Reps per day per exercise and arrange by Date and Reps
reps_per_day <- total_volume %>% 
  group_by(Date, Exercise) %>% 
  summarize(Total_Reps = sum(Reps)) %>% 
  arrange(Date, desc(Total_Reps))

View(reps_per_day)


##Sets per day by exercise, rename column from 'n' to 'Sets', and arrange by Date and Sets
sets_per_day <- total_volume %>% 
  group_by(Date) %>% 
  count(Exercise) %>% 
  rename(Sets = n) %>% 
  arrange(Date, desc(Sets))
  
View(sets_per_day)

##Max weight per exercise per day, arranged by Date and Weight
max_weight_per_exercise_day <- total_volume %>% 
  group_by(Date, Exercise) %>% 
  summarize(Max_Weight = max(Weight)) %>% 
  arrange(Date, desc(Max_Weight))

View(max_weight_per_exercise_day)


##One rep max per exercise per day using the Brzycki equation - weight × (36 / (37 - reps))
## Sorting for distinct results and arrange by Date and Highest 1RM
One_RM_per_exercise_day <- total_volume %>% 
  group_by(Date, Exercise) %>% 
  mutate(One_Rep_Max = Weight*(36/(37 - Reps))) %>% 
  slice_max(One_Rep_Max, n=1) %>% 
  distinct(One_Rep_Max) %>% 
  arrange(Date, desc(One_Rep_Max))

View(One_RM_per_exercise_day)




##Need to chart sets, reps, 1RM, max weight and total volume by date and wrap by exercise 
##so you can see progress of each movement over time, 
##set the date to be vertical so you can get each in view