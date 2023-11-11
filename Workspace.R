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
View(sets_per_exercise)

##Calculate Total Volume in lbs
total_volume <- date_sorted %>% 
  mutate(Total_Volume = Reps*Weight)
View(total_volume)

##Find how much volume per exercise per day, sort by date then desc volume
exercise_vol_by_day <- total_volume %>% 
  group_by(Date, Exercise) %>% 
  summarize(Sum_Total_Vol_LBs = sum(Total_Volume)) %>% 
  arrange(Date, desc(Sum_Total_Vol_LBs))


