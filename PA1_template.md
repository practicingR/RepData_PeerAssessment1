---
title: "Course 5:Reproducible Research - Peer Assignment 1"
output: 
  html_document:
   keep_md: true
---

## Number 1: Loading and processing the data


```r
library(dplyr)
library(ggplot2)
```

### Load the data


```r
originalData <- read.csv("~/R/R Course/Course 5/activity.csv")
```

### Remove NAs from the data


```r
activityData <- originalData[complete.cases(originalData), ]
```


## Number 2: What is mean total number of steps taken per day?

### Group data by day

```r
byday <- group_by(activityData, date)

stepsPerDay <- summarize(byday, total = sum(steps, na.rm = TRUE))
```

### Create a histogram showing total number of steps per day


```r
hist(stepsPerDay$total, main = "Total Steps Taken per Day", xlab = "Number of Steps per Day", ylab = "Frequency")
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png)<!-- -->

### Calculate mean and median of steps per day


```r
mean(stepsPerDay$total)
```

```
## [1] 10766.19
```

```r
median(stepsPerDay$total)
```

```
## [1] 10765
```

### The mean is 10766.19 and the median is 10765.

## Number 3: What is the average daily activity pattern?

### Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis).

#### First, calculate average steps per interval


```r
avgStepsPerInterval <- aggregate(steps ~ interval, activityData, mean)
```

#### Second, plot data as a time series plot


```r
plot(avgStepsPerInterval$interval, avgStepsPerInterval$steps, type = "l", main = "Average Steps Taken Across All Days", xlab = "5 Minute Interval", ylab = "Average Number of Steps Taken")
```

![](PA1_template_files/figure-html/unnamed-chunk-8-1.png)<!-- -->

### Find which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps

#### First, find maximum steps interval index


```r
maximumStepsRow <- which.max(avgStepsPerInterval$steps)
avgStepsPerInterval[maximumStepsRow,]
```

```
##     interval    steps
## 104      835 206.1698
```

#### Second, find values for maximum steps interval index


```r
maximumSteps <- avgStepsPerInterval[maximumStepsRow,1]
print(maximumSteps)
```

```
## [1] 835
```

### The maximum number of steps is 835

## Number 3: Imputing missing values

### Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)


```r
allNAs <- sum(is.na(originalData$steps))
allNAs
```

```
## [1] 2304
```

### There are 2304 NA values

### Create a function to return mean based on 5-minute interval



```r
getavgPerInterval <-function(interval){
    avgStepsPerInterval[avgStepsPerInterval$interval==interval,]$steps
}
```

### Create a new dataset that is equal to the original dataset but with the missing data filled in.


```r
newActivityData <- originalData

for (i in 1:nrow(newActivityData)) {
    if(is.na(newActivityData[i,]$steps)) {
        newActivityData[i,]$steps <- getavgPerInterval(newActivityData[i,]$interval)
    }
}
```

### Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day


```r
newStepsPerDay <- aggregate(steps ~ date, newActivityData, sum)

hist(newStepsPerDay$steps, main = "Total Steps Taken per Day (NAs replaced)", xlab = "Number of Steps per Day", ylab = "Frequency")
```

![](PA1_template_files/figure-html/unnamed-chunk-14-1.png)<!-- -->


## Number 4: Are there differences in activity patterns between weekdays and weekends?

### Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day


```r
newActivityData$date <- as.Date(strptime(newActivityData$date, format="%Y-%m-%d"))
newActivityData$day <- weekdays(newActivityData$date)
```

#### Create a fuunction to differentiate weekday and weekend - "If the day of week (DOW) is Saturday or Sunday mark as weekend, Else mark as weekday"


```r
for (i in 1:nrow(newActivityData)) {
  if(newActivityData[i,]$day %in% c("Saturday","Sunday")) {
    newActivityData[i,]$day <- "weekend"
  }
  else{newActivityData[i,]$day <- "weekday"
  }
}
```

#### Aggregate the average steps per day of week


```r
avgStepsPerDOW <- aggregate(steps ~ interval + day, newActivityData, mean)
```

### Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis).


```r
library(lattice)

xyplot(steps ~ interval | day, data = avgStepsPerDOW, type = "l", lwd = 2,
        layout = c(1, 2), 
        xlab = "Interval", 
        ylab = "Number of steps",
        main = "Avg Number of Steps from Weekday and Weekend days")
```

![](PA1_template_files/figure-html/unnamed-chunk-18-1.png)<!-- -->

### This displays both weekday and weekend average steps (code is correct and shows up in RStudio)

