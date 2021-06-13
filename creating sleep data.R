library(tidyr)
setwd('C:\\Users\\Mohamad\\Documents\\Syracuse\\Spring 2021\\IST 687\\project\\data\\condition')
DDF <- read.csv('condition_dataset.csv')
#using activity threshold of 40 for looking at total sleep <https://www.ncbi.nlm.nih.gov/pmc/articles/PMC3700718/>
nonActivity <- DDF[which(DDF$activity<=40),]
nonActivity$timestamp <- as.POSIXct(nonActivity$timestamp)

myTimeSubtractFun <- function(myDF){
  myVectOut <- NA
  for (i in 2:nrow(myDF)){
    myVectOut <- c(myVectOut, as.numeric(nonActivity$timestamp[i]-nonActivity$timestamp[i-1]))
  }
  return(myVectOut)}

nonActivity$minuteDiff <- myTimeSubtractFun(nonActivity)
write.csv(nonActivity, "sleep_data.csv")

someSleepDF <- nonActivity
someSleepDF$consecutive <- someSleepDF$minuteDiff<=2

#Grouping the data and getting sum of slept minutes a night
nonActivityCOpy <- someSleepDF
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

#Bipolar 2 Sleep
bipolarSleep <- combinedCondControlSleepDF[combinedCondControlSleepDF$afftype==1,]
bipolarSleepPlot <- ggplot(bipolarSleep, aes(x=DayOfWeek, y=hoursSlept))+
  geom_boxplot(aes(fill="red"), show.legend = FALSE)+
  xlab('Day of The Week')+
  ylab('Average hours slept per weekday')+
  ggtitle('Bipolar 2 - Amount of Sleep')+
  theme(plot.title = element_text(hjust=0.5, face='bold'))
bipolarSleepPlot

#MDD Sleep
majorDepressiveSleep <- combinedCondControlSleepDF[combinedCondControlSleepDF$afftype==2,]
mddSleepPlot <- ggplot(majorDepressiveSleep, aes(x=DayOfWeek, y=hoursSlept))+
  geom_boxplot(aes(fill="red"), show.legend = FALSE)+
  xlab('Day of The Week')+
  ylab('Average hours slept per weekday')+
  ggtitle('Major Depressive Disorde - Amount of Sleep')+
  theme(plot.title = element_text(hjust=0.5, face='bold'))
mddSleepPlot

#Bipolar1 sleep
bipolar1Sleep <- combinedCondControlSleepDF[combinedCondControlSleepDF$afftype==3,]
bp1SleepPlot <- ggplot(bipolar1Sleep, aes(x=DayOfWeek, y=hoursSlept))+
  geom_boxplot(aes(fill="red"), show.legend = FALSE)+
  xlab('Day of The Week')+
  ylab('Average hours slept per weekday')+
  ggtitle('Bipolar 1 - Amount of Sleep')+
  theme(plot.title = element_text(hjust=0.5, face='bold'))
bp1SleepPlot

library(gridExtra)
library(grid)
library(lattice)
fourGroupsSleep <- grid.arrange(controlSleepPlot, bp1SleepPlot, mddSleepPlot, bipolarSleepPlot, nrow=2)
fourGroupsSleep





#
#
#
#
#Classifying sleep based off night day is 8 AM to 8 PM
someSleepDF$hour <- as.numeric(format(someSleepDF$timestamp, format = "%H"))

nightSleep <- someSleepDF[((someSleepDF$hour>20) | (someSleepDF$hour<8)),]
daySleep <- someSleepDF[((someSleepDF$hour<21) & (someSleepDF$hour>7)),]



sleepAtNightDF <- nightSleep %>%
  group_by(number, date, age, gender, consecutive) %>% 
  summarise(SleepPerDay = sum(consecutive))
sleepAtNightDF$DayOfWeek <- weekdays(as.Date(sleepAtNightDF$date))

#Grouping the data and getting average slept minutes per day of the week
weeklySleepAtNightDf <- sleepAtNightDF %>%
  group_by(number, age, gender, DayOfWeek) %>% 
  summarise(DoWSleep = mean(SleepPerDay))

#ordering by the day of the week
weeklySleepAtNightDf$DayOfWeek <- factor(weeklySleepAtNightDf$DayOfWeek, levels= c(
  "Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))

weeklySleepAtNightDf <- weeklySleepAtNightDf[order(weeklySleepAtNightDf$number, weeklySleepAtNightDf$DayOfWeek),]

conditionWeeklySleepAtNightDf <- weeklySleepAtNightDf[1:70,]
conditionWeeklySleepAtNightDf <- merge(conditionWeeklySleepAtNightDf, scores, id='number')
conditionWeeklySleepAtNightDf$hoursSlept <- conditionWeeklySleepAtNightDf$DoWSleep/60


controlWeeklySleepAtNightDf <- weeklySleepAtNightDf[71:(nrow(weeklySleepAtNightDf)),]
controlWeeklySleepAtNightDf$hoursSlept <- controlWeeklySleepAtNightDf$DoWSleep/60
library(ggplot2)
#plotting control sleep
controlSleepAtNightPlot <- ggplot(controlWeeklySleepAtNightDf, aes(x=DayOfWeek, y=hoursSlept))+
  geom_boxplot(fill='sky blue')+
  xlab('Day of The Week')+
  ylab('Average hours slept per weekday')+
  ggtitle('Amount of Sleep for Control Group')+
  theme(plot.title = element_text(hjust=0.5, face='bold'))
controlSleepAtNightPlot
conditionWeeklySleepAtNightDf$afftype <- as.factor(conditionWeeklySleepAtNightDf$afftype)


#Creating one data frame with both Condtional and control 
combinedNightlySleepDF <- conditionWeeklySleepAtNightDf[,c(1,4,7,5,15)]
controlWeeklySleepAtNightDf$afftype <- as.factor(0)
controlWeeklySleepAtNightDf <- controlWeeklySleepDf[,c(1,4,7,5,6)]
combinedNightlySleepDF <- rbind(combinedNightlySleepDF, controlWeeklySleepAtNightDf)

#Bipolar 2 Sleep
bipolarSleepNight <- combinedNightlySleepDF[combinedNightlySleepDF$afftype==1,]
bipolarSleepPlotNight <- ggplot(bipolarSleepNight, aes(x=DayOfWeek, y=hoursSlept))+
  geom_boxplot(aes(fill="red"), show.legend = FALSE)+
  xlab('Day of The Week')+
  ylab('Average hours slept per night')+
  ggtitle('Bipolar 2 - Amount of Sleep At Night')+
  theme(plot.title = element_text(hjust=0.5, face='bold'))
bipolarSleepPlotNight

#MDD Sleep
majorDepressiveSleepNight <- combinedNightlySleepDF[combinedNightlySleepDF$afftype==2,]
mddSleepPlotNight <- ggplot(majorDepressiveSleepNight, aes(x=DayOfWeek, y=hoursSlept))+
  geom_boxplot(aes(fill="red"), show.legend = FALSE)+
  xlab('Day of The Week')+
  ylab('Average hours slept per night')+
  ggtitle('Major Depressive Disorde - Amount of Sleep At Night')+
  theme(plot.title = element_text(hjust=0.5, face='bold'))
mddSleepPlotNight

#Bipolar1 sleep
bipolar1SleepNight <- combinedNightlySleepDF[combinedNightlySleepDF$afftype==3,]
bp1SleepPlotNight <- ggplot(bipolar1SleepNight, aes(x=DayOfWeek, y=hoursSlept))+
  geom_boxplot(aes(fill="red"), show.legend = FALSE)+
  xlab('Day of The Week')+
  ylab('Average hours slept per night')+
  ggtitle('Bipolar 1 - Amount of Sleep At Night')+
  theme(plot.title = element_text(hjust=0.5, face='bold'))
bp1SleepPlotNight

library(gridExtra)
library(grid)
library(lattice)
fourGroupsSleepNight <- grid.arrange(controlSleepAtNightPlot, bp1SleepPlotNight, mddSleepPlotNight, bipolarSleepPlotNight, nrow=2)
fourGroupsSleepNight








#
#
#
#
#Classifying sleep based off day is 8 AM to 8 PM



sleepDayDF <- daySleep %>%
  group_by(number, date, age, gender, consecutive) %>% 
  summarise(SleepPerDay = sum(consecutive))
sleepDayDF$DayOfWeek <- weekdays(as.Date(sleepDayDF$date))

#Grouping the data and getting average slept minutes per day of the week
weeklySleepDayDf <- sleepDayDF %>%
  group_by(number, age, gender, DayOfWeek) %>% 
  summarise(DoWSleep = mean(SleepPerDay))

#ordering by the day of the week
weeklySleepDayDf$DayOfWeek <- factor(weeklySleepDayDf$DayOfWeek, levels= c(
  "Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))

weeklySleepDayDf <- weeklySleepDayDf[order(weeklySleepDayDf$number, weeklySleepDayDf$DayOfWeek),]

conditionWeeklySleepDayDf <- weeklySleepDayDf[1:70,]
conditionWeeklySleepDayDf <- merge(conditionWeeklySleepDayDf, scores, id='number')
conditionWeeklySleepDayDf$hoursSlept <- conditionWeeklySleepDayDf$DoWSleep/60


controlWeeklySleepDayDf <- weeklySleepDayDf[71:(nrow(weeklySleepDayDf)),]
controlWeeklySleepDayDf$hoursSlept <- controlWeeklySleepDayDf$DoWSleep/60
library(ggplot2)
#plotting control sleep
controlSleepDayPlot <- ggplot(controlWeeklySleepDayDf, aes(x=DayOfWeek, y=hoursSlept))+
  geom_boxplot(fill='sky blue')+
  xlab('Day of The Week')+
  ylab('Average hours slept per weekday')+
  ggtitle('Amount of Sleep for Control Group - Day')+
  theme(plot.title = element_text(hjust=0.5, face='bold'))
controlSleepDayPlot
conditionWeeklySleepDayDf$afftype <- as.factor(conditionWeeklySleepDayDf$afftype)


#Creating one data frame with both Condtional and control 
combinedDaySleepDF <- conditionWeeklySleepDayDf[,c(1,4,7,5,15)]
controlWeeklySleepDayDf$afftype <- as.factor(0)
controlWeeklySleepDayDf <- controlWeeklySleepDayDf[,c(1,4,7,5,6)]
combinedDayDayDF <- rbind(combinedDaySleepDF, controlWeeklySleepDayDf)

#Bipolar 2 Sleep
bipolarSleepDay <- combinedDayDayDF[combinedDayDayDF$afftype==1,]
bipolarSleepPlotDay <- ggplot(bipolarSleepDay, aes(x=DayOfWeek, y=hoursSlept))+
  geom_boxplot(aes(fill="red"), show.legend = FALSE)+
  xlab('Day of The Week')+
  ylab('Average hours slept')+
  ggtitle('Bipolar 2 - Amount of Sleep During Day')+
  theme(plot.title = element_text(hjust=0.5, face='bold'))
bipolarSleepPlotDay

#MDD Sleep
majorDepressiveSleepDay <- combinedDayDayDF[combinedDayDayDF$afftype==2,]
mddSleepPlotDay <- ggplot(majorDepressiveSleepDay, aes(x=DayOfWeek, y=hoursSlept))+
  geom_boxplot(aes(fill="red"), show.legend = FALSE)+
  xlab('Day of The Week')+
  ylab('Average hours slept')+
  ggtitle('Major Depressive Disorde - Amount of Sleep During Day')+
  theme(plot.title = element_text(hjust=0.5, face='bold'))
mddSleepPlotDay

#Bipolar1 sleep
bipolar1SleepDay <- combinedDayDayDF[combinedDayDayDF$afftype==3,]
bp1SleepPlotDay <- ggplot(bipolar1SleepDay, aes(x=DayOfWeek, y=hoursSlept))+
  geom_boxplot(aes(fill="red"), show.legend = FALSE)+
  xlab('Day of The Week')+
  ylab('Average hours slept')+
  ggtitle('Bipolar 1 - Amount of Sleep During Day')+
  theme(plot.title = element_text(hjust=0.5, face='bold'))
bp1SleepPlotDay

library(gridExtra)
library(grid)
library(lattice)
fourGroupsSleepDay <- grid.arrange(controlSleepDayPlot, bp1SleepPlotDay, mddSleepPlotDay, bipolarSleepPlotDay, nrow=2)
fourGroupsSleepDay

