## *Loading and preprocessing the data*
```{r}
setwd("/Users/adrianromano/Downloads")
activity <- read.csv("activity.csv")
str(activity)
activity$date <- as.Date(activity$date)
```

## *What is mean total number of steps taken per day?*

1. **Calculate the total number of steps taken per day**
```{r}
library(plyr)
totalSteps <- aggregate(activity$steps ~ activity$date, FUN = sum)
colnames(totalSteps) <- c("Date", "Steps")
head(totalSteps)
```

2. **Histogram of the total number of steps taken per day**
```{r}
hist(totalSteps$Steps, main = "Total Number of Steps", xlab = "Number of Steps", col = "red")
```

3. **Calculate the mean and median of the total number of steps taken per day**
```{r}
meanSteps <- mean(totalSteps$Steps, na.rm = TRUE)
meanSteps
medianSteps <- median(totalSteps$Steps, na.rm = TRUE)
medianSteps
```
* mean: 10766.19
* median: 10765

## *What is the average daily activity pattern?*

1. **Create a time series plot of the 5-minute interval and the average number of steps taken averaged across all days**
```{r}
averageSteps <- aggregate(activity$steps ~ activity$interval, FUN = mean, na.rm = TRUE)
colnames(averageSteps) <- c("Interval", "Steps")
head(averageSteps)
plot(averageSteps$Steps ~ averageSteps$Interval, 
                        type = "l", 
                        col = "blue", 
                        main = "Average Number of Steps per Interval", 
                        xlab = "Interval", 
                        ylab = "Average Number of Steps")
```

2. **Look at the maximum number of steps for the average 5-minute interval across all days**
```{r}
maxSteps <- averageSteps[which.max(averageSteps$Steps),]
maxSteps
```
* *Interval 835 has the maximum number of steps of 206.1698 for the average 5-minute interval across all days*

## *Imputing missing values*

1. **Look at the number of missing values in the data**
```{r}
sum(is.na(activity))
```
* *Total number of missing values is 2304*

2. **Replace NA Values with the mean average steps per interval**
```{r}
meanPerInterval <- mean(averageSteps$Steps, na.rm = TRUE)
activity1 <- activity
activity1$steps[is.na(activity1$steps)] <- meanPerInterval
```
* *Strategy: The missing values are replaced with the mean average steps per interval*

3. **Histogram of the total number of steps taken each day after missing values were imputed**
```{r}
totalSteps1 <- aggregate(activity1$steps ~ activity1$date, FUN = sum)
colnames(totalSteps1) <- c("Date", "Steps")
hist(totalSteps$Steps, main = "Total Number of Steps", xlab = "Number of Steps", col = "green")
```

4. **Calculate the mean and median of the total number of steps taken each day after missing values were imputed**
```{r}
meanSteps1 <- mean(totalSteps1$Steps)
meanSteps1
medianSteps1 <- median(totalSteps1$Steps)
medianSteps1
```
* mean: 10766.19
* median: 10766.19
* *Replacing the missing values did not have any change in the mean value and a slight increase in median value*

## *Are there differences in activity patterns between weekdays and weekends?*
1. **Create a new factor variable cating whether a given date is a weekday or weekend day**
```{r}
activity1$date <- as.Date(activity1$date)
activity1$dayCategory <- ifelse(weekdays(activity1$date) == "Saturday" | weekdays(activity1$date) == "Sunday", "Weekend", "Weekday")
averageDayCategory <- aggregate(activity1$steps ~ activity1$dayCategory + activity1$interval, FUN = mean)
colnames(averageDayCategory) <- c("DayType", "Interval", "Steps")
head(averageDayCategory)
```

2. **Make a panel plot containing a time series plot of the 5-minute interval and the average number of steps taken, averaged across all weekday days or weekend days**
```{r}
library(ggplot2)
p <- ggplot(averageDayCategory, aes(Interval, Steps, color = DayType))
p + geom_line() + labs(x = "Intervals", y = "Average Number of Steps", title = "Activity Patterns") + facet_wrap(~DayType,ncol=1,nrow=2)
```
