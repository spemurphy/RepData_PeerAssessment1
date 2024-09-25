---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---


## Loading and preprocessing the data


Reading in the code file


``` r
activity <- read.csv("/Users/seanmurphy/Desktop/DS_coursera/RepData_PeerAssessment1/activity.csv")
activity$date <- as.Date(activity$date)
# Creating a completely unchanged version of the data for later use
act_demo <- read.csv("/Users/seanmurphy/Desktop/DS_coursera/RepData_PeerAssessment1/activity.csv")
act_demo$date <- as.Date(activity$date)
```

## What is mean total number of steps taken per day?
Calculate the total number of steps taken per day


``` r
sum_steps <- aggregate(steps~date, activity, sum)
```

Histogram of the total number of steps taken each day


``` r
hist(sum_steps$steps, xlab="Total Daily Steps", ylim=c(0,30), main="Total Number of Steps Taken Each Day")
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

Calculate and report the mean steps taken per day


``` r
library(dplyr)
activity %>% group_by(date) %>% summarise(mean_steps=mean(steps))
```

```
## # A tibble: 61 × 2
##    date       mean_steps
##    <date>          <dbl>
##  1 2012-10-01     NA    
##  2 2012-10-02      0.438
##  3 2012-10-03     39.4  
##  4 2012-10-04     42.1  
##  5 2012-10-05     46.2  
##  6 2012-10-06     53.5  
##  7 2012-10-07     38.2  
##  8 2012-10-08     NA    
##  9 2012-10-09     44.5  
## 10 2012-10-10     34.4  
## # ℹ 51 more rows
```

and median of the total number of steps taken per day


``` r
activity %>% group_by(date) %>% summarise(median_steps=median(steps))
```

```
## # A tibble: 61 × 2
##    date       median_steps
##    <date>            <dbl>
##  1 2012-10-01           NA
##  2 2012-10-02            0
##  3 2012-10-03            0
##  4 2012-10-04            0
##  5 2012-10-05            0
##  6 2012-10-06            0
##  7 2012-10-07            0
##  8 2012-10-08           NA
##  9 2012-10-09            0
## 10 2012-10-10            0
## # ℹ 51 more rows
```



## What is the average daily activity pattern?

Make a time series plot of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)


``` r
avg_interval_steps <- activity %>%
  group_by(interval) %>%
  summarize(avg_steps = mean(steps, na.rm = TRUE))

plot(avg_interval_steps$interval, 
     avg_interval_steps$avg_steps, 
     type = "l",  
     xlab = "5-minute Interval", 
     ylab = "Average Number of Steps", 
     main = "Average Steps per 5-Minute Interval Across All Days"
) 
```

![](PA1_template_files/figure-html/unnamed-chunk-6-1.png)<!-- -->
## Conclusion
There is a large spike in step activity between 5-minute interval 750-1000, which likely equates to early-mid morning. 

Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?


``` r
max_interval_steps <- avg_interval_steps[which.max(avg_interval_steps$avg_steps), "interval"]
max_interval_steps
```

```
## # A tibble: 1 × 1
##   interval
##      <int>
## 1      835
```
## Conclusion
The 835 minute interval contains the maximum number of steps across all the days in the dataset. 


## Imputing missing values

Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)

``` r
sum(is.na(activity))
```

```
## [1] 2304
```

Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

Used the average steps per interval to fill in NA (missing) values as a solution for this problem. 

``` r
activity <- activity %>%
  group_by(interval) %>%
  mutate(avg_steps = mean(steps, na.rm = TRUE))
activity$steps <- ifelse(is.na(activity$steps), activity$avg_steps, activity$steps)
sum(is.na(activity$steps)) 
```

```
## [1] 0
```

New dataset that is equal to the original dataset but with the missing data filled in.

``` r
activity$steps <- ifelse(is.na(activity$steps), activity$avg_steps, activity$steps)
activity2 <- data.frame(activity)
activity2$avg_steps <- NULL
class(activity2)
```

```
## [1] "data.frame"
```
Histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. 


``` r
## Histogram
sum_steps2 <- aggregate(steps~date, activity2, sum)
hist(sum_steps2$steps, xlab="Total Daily Steps V2", ylim=c(0,40), main="Total Number of Steps Taken Each Day V2")
```

![](PA1_template_files/figure-html/unnamed-chunk-11-1.png)<!-- -->


``` r
## Calculating the mean and median steps for new dataframe and old dataframe.
act2_median_steps <- activity2 %>% group_by(date) %>% summarise(median_steps=median(steps))
act2_mean_steps <- activity2 %>% group_by(date) %>% summarise(mean_steps=mean(steps))
act1_median_steps <- activity %>% group_by(date) %>% summarise(median_steps=median(steps))
act1_mean_steps <- activity %>% group_by(date) %>% summarise(mean_steps=mean(steps))

## Calculating the difference in mean steps
act2_mean_steps$mean_steps - act1_mean_steps$mean_steps
```

```
##  [1] 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
## [57] 0 0 0 0 0
```

``` r
## Calculating the difference in median steps
act2_median_steps$median_steps - act1_median_steps$median_steps
```

```
##  [1] 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
## [57] 0 0 0 0 0
```

``` r
## Calculating the difference in total steps between the original and new dataset
difference_total_steps <- sum(activity2$steps) -sum(act_demo$steps, na.rm=TRUE)
difference_total_steps
```

```
## [1] 86129.51
```

## Conclusion
As mean steps for that day were used for replacement of NA values, the difference in mean and median is "0" or "NA". However, the total number of steps taken per day does show an 86,129.51 increase in total steps over the duration of the dataset.



## Are there differences in activity patterns between weekdays and weekends?

New factor variable in the dataset with two levels – “weekday” and “weekend”. 


``` r
# Create new column variable
activity2 <- activity2 %>%
  mutate(date = activity$date)

# Change format into days of the week 
activity2 <- activity2 %>%
  mutate(day=weekdays(activity$date))

# Change into factor variables to then change into numeric, for easier manipulation
activity2$day <- factor(activity2$day)
activity2$day <- as.numeric(activity2$day)

# Create a list 
day2 <- list()

# Loop through column, find weekdays and weekends, add to the new list 
for (day in 1:length(activity2$day)) {
  if (activity2$day[[day]] <= 2 | activity2$day[[day]] >= 5) {
    day2[[day]] <- "Weekday"
  }
  else {
    day2[[day]] <- "Weekend"
  }
}

# Swap old column with the new list delineating whether it is a weekend or a weekday
activity2$day <- day2

# Change back into factor variable 
activity3 <- as.data.frame(lapply(activity2, unlist))
activity3$day <- factor(activity3$day)
str(activity3$day)
```

```
##  Factor w/ 2 levels "Weekday","Weekend": 1 1 1 1 1 1 1 1 1 1 ...
```

Panel plot containing a time series plot (i.e.type = "l" of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis).


``` r
library(lattice)
# Filter the data by day and interval, then find the average steps.
activity3 <- activity3 %>% group_by(day, interval) %>% mutate(mean_steps=mean(steps))

# Plot the subsequent data
xyplot(mean_steps ~ interval | day, data=activity3, type="l", layout=c(1,2), 
       ylab="Average Steps", xlab="Interval Time", 
       main="Difference in Steps During Weekdays vs Weekends")
```

![](PA1_template_files/figure-html/unnamed-chunk-14-1.png)<!-- -->

## Conclusion
From the charts, there seems to be a larger amount of average steps during the start of the day on weekdays, while there is a more constant amount of average steps during weekends. 
