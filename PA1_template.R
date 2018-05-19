## Load and read the data
setwd("/Users/adrianromano/Downloads")
activity <- read.csv("activity.csv")
str(activity)

## What is mean total number of steps taken per day?
-----------------------------------------------------------------------------------------------------------------
## Calculate the total number of steps taken per day
library(plyr)
activity$date <- as.Date(activity$date)
totalSteps <- aggregate(activity$steps ~ activity$date, FUN = sum)
totalSteps
colnames(totalSteps) <- c("Date", "Steps")

## Histogram of the total number of steps taken per day
hist(totalSteps$Steps, main = "Total Number of Steps", xlab = "Number of Steps", col = "red")

## Calculate the mean and median of the total number of steps taken per day
meanSteps <- mean(totalSteps$Steps, na.rm = TRUE)
meanSteps
medianSteps <- median(totalSteps$Steps, na.rm = TRUE)
medianSteps

## What is the average daily activity pattern?
----------------------------------------------------------------------------------------------------------
## Create a time series plot of the 5-minute interval and the average number of steps taken averaged across all days
averageSteps <- aggregate(activity$steps ~ activity$interval, FUN = mean, na.rm = TRUE)
averageSteps
colnames(averageSteps) <- c("Interval", "Steps")
plot(averageSteps$Steps ~ averageSteps$Interval, 
                        type = "l", 
                        col = "blue", 
                        main = "Average Number of Steps per Interval", 
                        xlab = "Interval", 
                        ylab = "Average Number of Steps")

## Look at the maximum number of steps for the average 5-minute interval across all days
maxSteps <- averageSteps[which.max(averageSteps$Steps),]
maxSteps

## Imputing missing values
---------------------------------------------------------------------------------------------------------
## Look at the number of missing values in the data
sum(is.na(activity))

## Replace NA Values with the mean average steps per interval 
meanPerInterval <- mean(averageSteps$Steps, na.rm = TRUE)
activity1 <- activity
activity1$steps[is.na(activity1$steps)] <- meanPerInterval

## Histogram of the total number of steps taken each day after missing values were imputed
totalSteps1 <- aggregate(activity1$steps ~ activity1$date, FUN = sum)
colnames(totalSteps1) <- c("Date", "Steps")
hist(totalSteps$Steps, main = "Total Number of Steps", xlab = "Number of Steps", col = "green")

## Calculate the mean and median of the total number of steps taken each day after missing values were imputed
meanSteps1 <- mean(totalSteps1$Steps)
meanSteps1
medianSteps1 <- median(totalSteps1$Steps)
medianSteps1

## Are there differences in activity patterns between weekdays and weekends?
------------------------------------------------------------------------------------------------------------
activity1$date <- as.Date(activity1$date)
activity1$dayCategory <- ifelse(weekdays(activity1$date) == "Saturday" | weekdays(activity1$date) == "Sunday", "Weekend", "Weekday")
averageDayCategory <- aggregate(activity1$steps ~ activity1$dayCategory + activity1$interval, FUN = mean)
colnames(averageDayCategory) <- c("DayType", "Interval", "Steps")

library(ggplot2)
p <- ggplot(averageDayCategory, aes(Interval, Steps, color = DayType))
p + geom_line() + labs(x = "Intervals", y = "Number of Steps", title = "Activity Patterns") + facet_wrap(~DayType,ncol=1,nrow=2)
