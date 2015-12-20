# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data


```r
# Install required libraries first
library(ggplot2)
library(dplyr)
```

```
## 
## Attaching package: 'dplyr'
## 
## The following objects are masked from 'package:stats':
## 
##     filter, lag
## 
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

```r
# Unpack the data
unzip(zipfile="activity.zip")
data <- read.csv("activity.csv")

# Show first 5 records
head(data, 5)
```

```
##   steps       date interval
## 1    NA 2012-10-01        0
## 2    NA 2012-10-01        5
## 3    NA 2012-10-01       10
## 4    NA 2012-10-01       15
## 5    NA 2012-10-01       20
```

## What is mean total number of steps taken per day?

 1. Calculate the total number of steps taken per day


```r
stepsPerDay <- tapply(data$steps, data$date, FUN=sum, na.rm=TRUE)
steps.df <- as.data.frame(stepsPerDay)
# Display some records
head(steps.df, 10)
```

```
##            stepsPerDay
## 2012-10-01           0
## 2012-10-02         126
## 2012-10-03       11352
## 2012-10-04       12116
## 2012-10-05       13294
## 2012-10-06       15420
## 2012-10-07       11015
## 2012-10-08           0
## 2012-10-09       12811
## 2012-10-10        9900
```

 2. If you do not understand the difference between a histogram and a barplot, research the difference between them. Make a histogram of the total number of steps taken each day


```r
hist(stepsPerDay, breaks=11, xlab="Steps per day", main="Total steps per day")
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png) 

 3. Calculate and report the mean and median of the total number of steps taken per day
 

```r
steps.mean <- mean(stepsPerDay, na.rm=TRUE)
steps.mean
```

```
## [1] 9354.23
```

```r
steps.median <- median(stepsPerDay, na.rm=TRUE)
steps.median
```

```
## [1] 10395
```

## What is the average daily activity pattern?

 1. Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)


```r
avgs <- tapply(data$steps, data$interval, mean, na.rm=T)
avgsInterval <- data %>% group_by(interval) %>% summarise(meanSteps = mean(steps, na.rm = TRUE))
ggplot(data = as.data.frame(avgsInterval), mapping = aes(x = interval, y = meanSteps)) + 
    geom_line() + scale_x_continuous("Day Interval", breaks = seq(min(avgsInterval$interval), 
    max(avgsInterval$interval), 100)) + scale_y_continuous("Avg Number of Steps") + 
    ggtitle("Avg Number of Steps by Interval")
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png) 

 2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?
 

```r
data$interval[which(avgs==max(avgs))]
```

```
## [1] 835
```

## Imputing missing values

 1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)


```r
rowsWithNAs <- nrow(data) - (nrow(data %>% na.omit()))
rowsWithNAs
```

```
## [1] 2304
```
 2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

  Inserting the average for each interval
  
 3. Create a new dataset that is equal to the original dataset but with the missing data filled in.


```r
steps_by_interval <- aggregate(steps ~ interval, data, mean)
data.without.na <- transform(data, steps = ifelse(is.na(data$steps), steps_by_interval$steps[match(data$interval, steps_by_interval$interval)], data$steps))
```
 4. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?


```r
stepsPerDay <- tapply(data.without.na$steps, data.without.na$date, FUN=sum)
qplot(stepsPerDay, binwidth=1000, xlab="Total number of steps per day")
```

![](PA1_template_files/figure-html/unnamed-chunk-9-1.png) 

```r
mean(stepsPerDay)
```

```
## [1] 10766.19
```

```r
median(stepsPerDay)
```

```
## [1] 10766.19
```

## Are there differences in activity patterns between weekdays and weekends?

 1. Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.
 

```r
data.without.na$weekdayType <- ifelse(weekdays(as.Date(data.without.na$date)) %in% c("Satuday", "Sunday"), "weekend", "weekday")
data.without.na <- data.without.na %>% group_by(interval, weekdayType) %>% summarise(meanSteps = mean(steps, 
    na.rm = TRUE))
```

 2. Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.
 

```r
ggplot(data = data.without.na, mapping = aes(x = interval, y = meanSteps)) + 
    geom_line() + facet_grid(weekdayType ~ .) + scale_x_continuous("Day Interval", 
    breaks = seq(min(data.without.na$interval), max(data.without.na$interval), 100)) + 
    scale_y_continuous("Average Number of Steps") + ggtitle("Average Number of Steps Taken by Interval")
```

![](PA1_template_files/figure-html/unnamed-chunk-11-1.png) 
