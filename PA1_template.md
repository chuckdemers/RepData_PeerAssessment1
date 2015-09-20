# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data

Load the data

```r
d <- read.csv("activity.csv")
```
Examine the format of the data

```r
str(d)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : Factor w/ 61 levels "2012-10-01","2012-10-02",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```

```r
head(d)
```

```
##   steps       date interval
## 1    NA 2012-10-01        0
## 2    NA 2012-10-01        5
## 3    NA 2012-10-01       10
## 4    NA 2012-10-01       15
## 5    NA 2012-10-01       20
## 6    NA 2012-10-01       25
```

Change the dates from Factor to Dates

```r
d$date <- as.Date(d$date)
```
Verify change

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : Date, format: "2012-10-01" "2012-10-01" ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```

## What is mean total number of steps taken per day?

**1. Calculate the total number of steps taken per day**


```r
library(dplyr)
steps_df <- d %>%
    group_by(date) %>%
    summarize( tsteps = sum(steps, na.rm=TRUE))
```

**2. If you do not understand the difference between a histogram and a barplot, research the difference between them. Make a histogram of the total number of steps taken each day**


```r
library(ggplot2)
ggplot(data=d, aes(d$steps)) + 
    geom_histogram( ) +
    ylab("Number of occurances of Step Count") +
    xlab("Number of Steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png) 

**3. Calculate and report the mean and median of the total number of steps taken per day**


```r
mean_steps = mean(d$steps, na.rm=TRUE)
```

The mean of the steps taken per day is 37.3825996.


```r
median_steps = median(d$steps, na.rm=TRUE)
```

The median of the steps taken per day is 0.

## What is the average daily activity pattern?

```r
asteps_df <- d %>%
    group_by(interval) %>%
    summarize( asteps = mean(steps, na.rm=TRUE))
```

**1. Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)**


```r
plot( asteps_df$interval, asteps_df$asteps, 
      type="n",
      xlab="Interval",
      ylab="Average Steps"
      )
lines(asteps_df$interval, asteps_df$asteps, type="l" )
```

![](PA1_template_files/figure-html/average_steps_plot-1.png) 

**2.Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?**


```r
m = asteps_df[asteps_df$asteps == max( asteps_df$asteps),]
```

The interval with the most average steps is 835.

## Imputing missing values

**Note that there are a number of days/intervals where there are missing values (coded as NA). The presence of missing days may introduce bias into some calculations or summaries of the data.**

**1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)**

The total number of rows with missing values is 2304.

**2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.**

The strategy to fill in NA values for the number of steps will be to use the rounded mean of the 5-minute interval.

**3. Create a new dataset that is equal to the original dataset but with the missing data filled in.**


```r
nd <- d
#print(nrow(nd))
#head(asteps_df)
for (i in 1:nrow(nd)) {
  if (is.na(nd[i,"steps"])){
    interval <- nd[i,"interval"]
    new_steps <- as.numeric(round(asteps_df[asteps_df[,"interval"]==interval,"asteps"]))
    nd[i,"steps"] <- new_steps
    }
}
#head(nd)
```

**4. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?**


```r
library(ggplot2)
sub_plot <- ggplot(data=nd, aes(nd$steps)) + 
    geom_histogram( ) +
    ylab("Number of occurances of Step Count") +
    xlab("Number of Steps") +
    ggtitle("Replaced NA Values")
```

The histogram with the substited values.


```r
print(sub_plot)
```

![](PA1_template_files/figure-html/showsubplot-1.png) 


```r
sub_mean_steps = mean(nd$steps, na.rm=T)
sub_mean_steps
```

```
## [1] 37.38069
```

The mean of the steps taken per day is 37.3806922.


```r
sub_median_steps = median(nd$steps)
```

The median of the steps taken per day is 0.

## Are there differences in activity patterns between weekdays and weekends?

**1. Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.**


```r
weekend <- c("Saturday","Sunday")
nd$daytype <- with(nd, ifelse( weekdays(date) %in% weekend, "weekend","weekday"))
```

**2. Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.**


```r
wday <- subset(nd, daytype=="weekday")
weekday_asteps <- wday %>%
    group_by(interval) %>%
    summarize( asteps = mean(steps, na.rm=TRUE))
head(weekday_asteps)
```

```
## Source: local data frame [6 x 2]
## 
##   interval     asteps
## 1        0 2.28888889
## 2        5 0.40000000
## 3       10 0.15555556
## 4       15 0.17777778
## 5       20 0.08888889
## 6       25 1.57777778
```

```r
wend <- subset(nd, daytype=="weekend")
weekend_asteps <- wend %>%
    group_by(interval) %>%
    summarize( asteps = mean(steps, na.rm=TRUE))

par(mfrow=c(2,1))
plot( weekend_asteps$interval, weekend_asteps$asteps, 
      type="l",
      xlab="Interval",
      ylab="Average Steps"
      )
plot( weekday_asteps$interval, weekday_asteps$asteps, 
      type="l",
      xlab="Interval",
      ylab="Average Steps"
      )
```

![](PA1_template_files/figure-html/splitdata-1.png) 
