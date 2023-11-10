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

##Transform Date from Character to Date format
class(fulldata$Date)
fulldata$Date <- mdy(fulldata$Date)

##Check change
class(fulldata$Date)

##Sort by Date
date_sorted <- arrange(fulldata,Date)
View(date_sorted)



