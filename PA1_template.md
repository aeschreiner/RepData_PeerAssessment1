---
---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---

# Reproducible Research: Peer Assessment 1

## Loading and preprocessing the data

The activity data is loaded from a CSV-formatted file: 'activity.csv', with the columns:

* steps: Number of steps taking in a 5-minute interval (missing values are coded as NA)
* date: The date on which the measurement was taken in YYYY-MM-DD format
* interval: Identifier for the 5-minute interval in which measurement was taken

Read the data. Make use of the dplyr and ggplot2 libraries in all the following.


```r
require(dplyr)
require(ggplot2)

activity <- read.csv('activity.csv')
```

## What is mean total number of steps taken per day?

To calculate the mean and median number of steps per day, we group the activity data by date, and then summarize the grouped data with the sum function.


```r
stepsd <- activity %>% group_by(date) %>% summarize(sum=sum(steps))

g <- ggplot(stepsd,aes(sum)) +
    geom_histogram(binwidth=1000,color='black',fill='blue') +
    theme_bw() +
    labs(title='Distribution of Steps per Day') +
    labs(x='Steps per Day')
print(g)
```

![plot of chunk steps_per_day](figure/steps_per_day-1.png) 

Compute the mean number of steps per day,


```r
mean(stepsd$sum,na.rm=T)
```

```
## [1] 10766.19
```

and the median number of steps per day.


```r
median(stepsd$sum,na.rm=T)
```

```
## [1] 10765
```

## What is the average daily activity pattern?

To calculate the average pattern of steps per 5-minute interval, group the data by the interval column and then summarize with the mean function (and remove NA values). The plot's x-axis is interval but more properly, the interval should be comverted to a continuouos time of day, because the interval value has jumps at 55,100; 155,200; etc ...


```r
stepsi <- activity %>% group_by(interval) %>% summarize(mean=mean(steps,na.rm=TRUE))

g <- ggplot(stepsi,aes(interval,mean)) +
    geom_line(color='blue') +
    theme_bw() +
    labs(title='Average Steps per Interval') +
    labs(x='Interval') +
    labs(y='Number of Steps')
print(g)
```

![plot of chunk mean_daily](figure/mean_daily-1.png) 


```r
maxint <- stepsi$interval[stepsi$mean==max(stepsi$mean)]
```

The maximum number of average steps taken occurs at interval 835.


## Imputing missing values

There are number of date/interval entries that have missing values, coded as NA.


```r
nentry <- length(activity$steps)
nmissing <- sum(is.na(activity$steps))
```

The number of missing values is 2304, out of 17568 entries.

Create a new activity dataset and replace missing values, with the mean value over all days at the corresponding interval. Then summarize the new dataset by day as before and show the histogram of steps per day.


```r
activity2 <- activity
for (i in seq(length(stepsi$interval))) {
    int <- stepsi$interval[i]
    intmean <- floor(stepsi$mean[i])
    index <- which(activity2$interval == int & is.na(activity2$steps))
    activity2$steps[index] <- intmean
    }

stepsd2 <- activity2 %>% group_by(date) %>% summarize(sum=sum(steps))

g <- ggplot(stepsd2,aes(sum)) +
    geom_histogram(binwidth=1000,color='black',fill='blue') +
    theme_bw() +
    labs(title='Distribution of Steps per Day\n(with inputed missing values)') +
    labs(x='Steps per Day')
print(g)
```

![plot of chunk replace_missing](figure/replace_missing-1.png) 

Compute the mean number of steps per day,


```r
mean(stepsd2$sum)
```

```
## [1] 10749.77
```

and the median number of steps per day.


```r
median(stepsd2$sum)
```

```
## [1] 10641
```

The computed mean and median values of the daily number of steps changes when the missing values of the original date are replaced, by the mean valued per the corresponding interval.


## Are there differences in activity patterns between weekdays and weekends?

Modify the dataset with filled-in values with a new column evaluating either 'weekday' or 'weekend' depending on the day of the week. Plot the mean per interval, comparing weekday vs. weekend.


```r
activity2 <- activity2 %>%
    mutate(workday=ifelse(weekdays(as.Date(date)) %in% c('Saturday','Sunday'),'weekend','weekday'))

stepsi2 <- activity2 %>% group_by(interval,workday) %>% summarize(mean=mean(steps))

g <- ggplot(stepsi2,aes(interval,mean)) +
    facet_wrap(~workday,ncol=1) +
    geom_line(color='blue') +
    theme_bw() +
    labs(title='Average Steps per Interval') +
    labs(x='Interval') +
    labs(y='Number of Steps')
print(g)
```

![plot of chunk weekday](figure/weekday-1.png) 