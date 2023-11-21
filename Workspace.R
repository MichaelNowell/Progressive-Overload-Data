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
  theme(axis.text.x = element_text(angle = 60, vjust = 1.0, hjust=1))+
  labs(title ="Total Sets per Exercise", subtitle = "Which Exercises Had The Most Volume")+
  theme(plot.title= element_text(hjust = .5))+
  theme(plot.subtitle= element_text(hjust = .5))+
  scale_y_continuous(breaks = waiver(), n.breaks = 15)


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


##Charts 2-6 to show progress for each exercise over time through various metrics, 
##including Reps, Sets, Max Weight, One Rep Max, and Volume
##
reps_per_day %>% 
  ggplot(mapping = aes(Date, Total_Reps))+
  geom_line()+
  labs(title ="Rep Progression Over Time", x="Month", y="Reps per Workout")+
  theme(plot.title= element_text(hjust = .5))+
  scale_x_date(date_breaks="1 month", date_labels="%b,%y")+
  theme(axis.text.x = element_text(angle = 75, vjust = 1.0, hjust=1))+
  scale_y_continuous(breaks = waiver(), n.breaks = 5)+
  facet_wrap(~Exercise)
 
sets_per_day %>% 
  ggplot(mapping = aes(Date, Sets))+
  geom_line()+
  labs(title ="Sets per Workout Over Time", x="Month", y="Sets per Workout")+
  theme(plot.title= element_text(hjust = .5))+
  scale_x_date(date_breaks="1 month", date_labels="%b,%y")+
  theme(axis.text.x = element_text(angle = 75, vjust = 1.0, hjust=1))+
  scale_y_continuous(breaks = waiver(), n.breaks = 5)+
  facet_wrap(~Exercise)

max_weight_per_exercise_day %>% 
  ggplot(mapping = aes(Date, Max_Weight))+
  geom_line()+
  labs(title ="Max Weight Lifted Each Workout", x="Month", y="Max Weight per Workout")+
  theme(plot.title= element_text(hjust = .5))+
  scale_x_date(date_breaks="1 month", date_labels="%b,%y")+
  theme(axis.text.x = element_text(angle = 75, vjust = 1.0, hjust=1))+
  scale_y_continuous(breaks = waiver(), n.breaks = 6)+
  facet_wrap(~Exercise)
  

One_RM_per_exercise_day %>% 
  ggplot(mapping = aes(Date, One_Rep_Max))+
  geom_line()+
  labs(title ="One Rep Maximum Over Time", x="Month", y="Estimated 1RM per Workout")+
  theme(plot.title= element_text(hjust = .5))+
  scale_x_date(date_breaks="1 month", date_labels="%b,%y")+
  theme(axis.text.x = element_text(angle = 75, vjust = 1.0, hjust=1))+
  ylim(0, 1800)+
  facet_wrap(~Exercise)

exercise_vol_by_day %>% 
  ggplot(mapping = aes(Date, Sum_Total_Vol_LBs))+
  geom_line()+
  labs(title ="Total Workout Volume in LBs Over Time", x="Month", y="Total Volume per Workout")+
  theme(plot.title= element_text(hjust = .5))+
  scale_x_date(date_breaks="1 month", date_labels="%b,%y")+
  theme(axis.text.x = element_text(angle = 75, vjust = 1.0, hjust=1))+
  scale_y_continuous(breaks = waiver(), n.breaks = 6)+
  facet_wrap(~Exercise)


##Chart 7 - Smooth Trend Line for Sets/Reps/1RM/Max Weight to compare outcomes for each

ggplot() + 
  geom_line(data=reps_per_day, aes(x=Date, y=Total_Reps, colour="Reps")) + 
  geom_line(data=sets_per_day, aes(x=Date, y=Sets, colour='Sets'))+
  geom_line(data=One_RM_per_exercise_day, aes(x=Date, y=One_Rep_Max, colour='One_Rep_Max')) +
  geom_line(data=max_weight_per_exercise_day, aes(x=Date, y=Max_Weight, colour='Max_Weight')) +
  ylim(0,1600)+
  labs(title ="Trend Lines for Sets/Reps/1RM/Max Weight Over Time", x="Month", y="Total Number")+
  theme(plot.title= element_text(hjust = .5))+
  scale_x_date(date_breaks="1 month", date_labels="%b,%y")+
  theme(axis.text.x = element_text(angle = 75, vjust = 1.0, hjust=1))+
  geom_smooth(method = "lm")+
  facet_wrap(~Exercise)+
  scale_color_manual(name='Metrics',
                     breaks=c('Reps', 'Sets', 'One_Rep_Max', 'Max_Weight'),
                     values=c('Reps'='red', 'Sets'='blue', 'One_Rep_Max'='purple', Max_Weight = 'green'))



 
## Overall trend shows that lifts remained flat in general with the exception of calf exercises
## This could be a function of losing weight during this time period
## Indicated overall neep for more consistent tracking and Progressive Overloading