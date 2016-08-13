---
title: "RepDataPeerAssesment1"
author: "Juho Pesonen"
date: "12 elokuuta 2016"
output: html_document
---


## R Markdown

This is my assignment for Reproductible Research course, Peer Assesment 1
```{r, echo=TRUE}
##reading data
activities<-read.csv(file="activity.csv", head=TRUE, sep=",") ##reading data in the folder, must be unpacked .csv data
```

##What is mean total number of steps taken per day?

For this part of the assignment, you can ignore the missing values in the dataset.

1.Make a histogram of the total number of steps taken each day

2.What is the average daily activity pattern?

Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?Calculate and report the mean and median total number of steps taken per day

```{r, echo=TRUE}
activities2<-na.omit(activities) ##omitting NA rows
steps_day<-aggregate(steps~date, activities2, sum) ##aggregating sum of total number of steps
hist(steps_day$steps, main="Number of days with different number of total steps each day", xlab="Total number of steps per day") ##creating histogram
mean(steps_day$steps) ##mean total steps
median(steps_day$steps) ##median total steps
```

##What is the average daily activity pattern?

1. Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```{r, echo=TRUE}
interval_steps<-aggregate(steps~interval, activities2, mean)
plot(interval_steps$interval, interval_steps$steps, type="l", main="Average number of steps taken, averaged accross all days", ylab="Average number of steps", xlab="Interval")
row_id<-which.max(interval_steps$steps)
interval_steps[row_id,]
```
Interval 835 has 206 steps which is the maximum across all data. 

##Imputing missing values

1. Note that there are a number of days/intervals where there are missing values (coded as NA). The presence of missing days may introduce bias into some calculations or summaries of the data.

2. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)

3. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

4. Create a new dataset that is equal to the original dataset but with the missing data filled in.

5. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?
```{r, echo=TRUE}
summary(activities$steps)
```
2304 rows with missing values

Filling missing step values with each interval groups median
```{r, echo=TRUE}
library(plyr)
impute <- function(x, fun) {
  missing <- is.na(x)
  replace(x, missing, fun(x[!missing]))
}
new_activities<- ddply(activities, ~ interval, transform, steps = impute(steps, median))

steps_day2<-aggregate(steps~date, new_activities, sum)
hist(steps_day2$steps, main="Number of days with different number of total steps each day", xlab="Total number of steps per day")
mean(steps_day2$steps)
median(steps_day2$steps)
```
When missing values in steps are replaced with the mean steps of each interval, it seems that there are a lot more days with less than 5000 steps taken. It also lowers the mean and median daily steps, especially the mean score.


##Are there differences in activity patterns between weekdays and weekends?

For this part the weekdays() function may be of some help here. Use the dataset with the filled-in missing values for this part.

1. Create a new factor variable in the dataset with two levels -- "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.

2. Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). 

```{r}
library(ggplot2)
mydate<-as.Date(new_activities$date, format="%Y-%m-%d")

daytype <- function(date) {
  if (weekdays(as.Date(date)) %in% c("lauantai", "sunnuntai")) { ##lauantai is Saturday and sunnuntai is Sunday in Finnish
    "weekend"
  } else {
    "weekday"
  }
}
new_activities$daytype <- as.factor(sapply(new_activities$date, daytype))
interval_steps2<-aggregate(steps~interval+daytype, new_activities, mean)
qplot(interval,steps,data=interval_steps2, geom=c("line"), xlab="Interval", ylab="Number of steps", main="Average number of steps during weekdays and weekends")+facet_wrap(~daytype, ncol=1)
```

It seems that during weekdays the days start around six o'clock in the morning whereas during weekends one can sleep longer. Data also shows that the person is more active during the weekends than during weekdays