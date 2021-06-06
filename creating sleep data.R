library(tidyr)
setwd('C:\\Users\\Mohamad\\Documents\\Syracuse\\Spring 2021\\IST 687\\project\\data\\condition')
DDF <- read.csv('condition_dataset.csv')
#using activity threshold of 40 for looking at total sleep <https://www.ncbi.nlm.nih.gov/pmc/articles/PMC3700718/>
nonActivity <- DDF[which(DDF$activity<=40),]
nonActivity$consecutive <- c(NA,diff(as.Date(nonActivity$timestamp))<3)
nonActivity <- nonActivity[,c(1:5,7,14)]
nonActivity <- na.omit(nonActivity)

#Grouping the data and getting sum of slept minutes a night
nonActivityCOpy <- nonActivity
library(dplyr)

sleepDF <- nonActivityCOpy %>%
  group_by(number, date, age, gender, consecutive) %>% 
  summarise(SleepPerDay = sum(consecutive))
sleepDF$DayOfWeek <- weekdays(as.Date(sleepDF$date))

#Grouping the data and getting average slept minutes per day of the week
weeklySleepDf <- sleepDF %>%
  group_by(number, age, gender, DayOfWeek) %>% 
  summarise(DoWSleep = mean(SleepPerDay))

#ordering by the day of the week
weeklySleepDf$DayOfWeek <- factor(weeklySleepDf$DayOfWeek, levels= c(
  "Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))

weeklySleepDf <- weeklySleepDf[order(weeklySleepDf$number, weeklySleepDf$DayOfWeek),]

conditionWeeklySleepDf <- weeklySleepDf[1:70,]
conditionWeeklySleepDf <- merge(conditionWeeklySleepDf, scores, id='number')
conditionWeeklySleepDf$hoursSlept <- conditionWeeklySleepDf$DoWSleep/60


controlWeeklySleepDf <- weeklySleepDf[71:(nrow(weeklySleepDf)),]
controlWeeklySleepDf$hoursSlept <- controlWeeklySleepDf$DoWSleep/60
library(ggplot2)
#plotting control sleep
controlSleepPlot <- ggplot(controlWeeklySleepDf, aes(x=DayOfWeek, y=hoursSlept))+
  geom_boxplot(fill='sky blue')+
  xlab('Day of The Week')+
  ylab('Average hours slept per weekday')+
  ggtitle('Amount of Sleep for Control Group')+
  theme(plot.title = element_text(hjust=0.5, face='bold'))
controlSleepPlot
conditionWeeklySleepDf$afftype <- as.factor(conditionWeeklySleepDf$afftype)


#plotting conditional sleep
conditionalSleepPlot <- ggplot(conditionWeeklySleepDf, aes(x=DayOfWeek, y=hoursSlept, fill=afftype))+
  geom_boxplot()+
  #facet_grid(afftype ~ DayOfWeek) +
  xlab('Day of The Week')+
  ylab('Average hours slept per night')+
  ggtitle('Amount of Sleep for Condtitional Group')+
  scale_fill_discrete(
    limits = factor(c(1, 2, 3)),
    labels = c("Bipolar 2", "Major Depressive Dissorder", "Bipolar 1"))+
  theme(plot.title = element_text(hjust=0.5, face='bold'))
conditionalSleepPlot


#Creating one data frame with both Condtional and control 
combinedCondControlSleepDF <- conditionWeeklySleepDf[,c(1,4,7,5,15)]
controlWeeklySleepDf$afftype <- as.factor(0)
controlWeeklySleepDfCopy <- controlWeeklySleepDf[,c(1,4,7,5,6)]
combinedCondControlSleepDF <- rbind(combinedCondControlSleepDF, controlWeeklySleepDfCopy)


#plotting conditional and control sleep together
combinedSleepPlot <- ggplot(combinedCondControlSleepDF, aes(x=DayOfWeek, y=hoursSlept, fill=afftype))+
  geom_boxplot()+
  #facet_grid(afftype ~ DayOfWeek) +
  xlab('Day of The Week')+
  ylab('Average hours slept per night')+
  ggtitle('Amount of Sleep for All Groups')+
  scale_fill_discrete(
    limits = factor(c(0, 1, 2, 3)),
    labels = c("Control", "Bipolar 2", "Major Depressive Dissorder", "Bipolar 1"))+
  theme(plot.title = element_text(hjust=0.5, face='bold'))
combinedSleepPlot


