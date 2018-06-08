---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---


```
## 
## Attaching package: 'dplyr'
```

```
## The following objects are masked from 'package:stats':
## 
##     filter, lag
```

```
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```


## Loading and preprocessing the data
Assume the source file is in the current directory.  
If it is in another folder, setwd() can be used in the R script below (added the commented line for clarity).


```r
#setwd("C:/work/data_science/Course5_Reprod/data")
act <- read.csv("activity.csv")
act$date <- as.Date(act$date, '%Y-%m-%d')
head(act, 20)
```

```
##    steps       date interval
## 1     NA 2012-10-01        0
## 2     NA 2012-10-01        5
## 3     NA 2012-10-01       10
## 4     NA 2012-10-01       15
## 5     NA 2012-10-01       20
## 6     NA 2012-10-01       25
## 7     NA 2012-10-01       30
## 8     NA 2012-10-01       35
## 9     NA 2012-10-01       40
## 10    NA 2012-10-01       45
## 11    NA 2012-10-01       50
## 12    NA 2012-10-01       55
## 13    NA 2012-10-01      100
## 14    NA 2012-10-01      105
## 15    NA 2012-10-01      110
## 16    NA 2012-10-01      115
## 17    NA 2012-10-01      120
## 18    NA 2012-10-01      125
## 19    NA 2012-10-01      130
## 20    NA 2012-10-01      135
```



## What is mean total number of steps taken per day?

Get the total steps per each day - ignoring NAs are per the assignment:


```r
tsteps <- aggregate(steps ~ date, data = act, sum, na.rm = TRUE)
head(tsteps)
```

```
##         date steps
## 1 2012-10-02   126
## 2 2012-10-03 11352
## 3 2012-10-04 12116
## 4 2012-10-05 13294
## 5 2012-10-06 15420
## 6 2012-10-07 11015
```

Draw a histogram of the steps taken per day:

```r
hist(tsteps$steps
     , xlab = "Steps"
     , main = "Histogram of steps per day (NAs excluded)"
     , breaks = 25
     , col = "red"
     )
```

![](PA1_template_files/figure-html/hist-1.png)<!-- -->

Get the Mean:

```r
mean(tsteps$steps)
```

```
## [1] 10766.19
```

Get the Median:

```r
median(tsteps$steps)
```

```
## [1] 10765
```



## What is the average daily activity pattern?

Plot average steps per interval:

```r
agg <- aggregate(steps ~ interval, data = act, mean, na.rm = TRUE)
plot(agg$interval
    ,agg$steps
    ,type = "l"
    ,xlab = "Interval"
    ,ylab = "Total steps"
    ,main = "Average steps per interval"
     )
```

![](PA1_template_files/figure-html/plot_avg-1.png)<!-- -->

Show the interval with max number of steps:

```r
agg[which.max(agg$steps),1]
```

```
## [1] 835
```

## Imputing missing values

Total number of missing values (NAs):

```r
sum(is.na(act))
```

```
## [1] 2304
```

To impute missing values, use an average per interval and plug into the main data frame:

```r
act_agg <- merge(act, agg, by = "interval")
act_agg$steps <- act_agg$steps.x
suppressWarnings(
    act_agg$steps[is.na(act_agg$steps.x) == TRUE] <- act_agg$steps.y[is.na(act_agg$steps.x) == TRUE]
)
```

Create new dataset with missing data filled in:

```r
act_clean <- arrange(select(act_agg, steps, date, interval), date, interval)
head(act_clean, 20)
```

```
##        steps       date interval
## 1  1.7169811 2012-10-01        0
## 2  0.3396226 2012-10-01        5
## 3  0.1320755 2012-10-01       10
## 4  0.1509434 2012-10-01       15
## 5  0.0754717 2012-10-01       20
## 6  2.0943396 2012-10-01       25
## 7  0.5283019 2012-10-01       30
## 8  0.8679245 2012-10-01       35
## 9  0.0000000 2012-10-01       40
## 10 1.4716981 2012-10-01       45
## 11 0.3018868 2012-10-01       50
## 12 0.1320755 2012-10-01       55
## 13 0.3207547 2012-10-01      100
## 14 0.6792453 2012-10-01      105
## 15 0.1509434 2012-10-01      110
## 16 0.3396226 2012-10-01      115
## 17 0.0000000 2012-10-01      120
## 18 1.1132075 2012-10-01      125
## 19 1.8301887 2012-10-01      130
## 20 0.1698113 2012-10-01      135
```

Histogram with the clean dataset:

```r
tsteps <- aggregate(steps ~ date, data = act_clean, sum, na.rm = TRUE)
hist(tsteps$steps
     , xlab = "Steps"
     , main = "Histogram of steps per day (NAs imputed)"
     , breaks = 25
     , col = "green"
     )
```

![](PA1_template_files/figure-html/hist_clean-1.png)<!-- -->

Get the Mean:

```r
mean(tsteps$steps)
```

```
## [1] 10766.19
```

Get the Median:

```r
median(tsteps$steps)
```

```
## [1] 10766.19
```

There is no significant impact of imputing missing values on the estimates.  

## Are there differences in activity patterns between weekdays and weekends?

Create a new factor variable we_fact to indicate weekend vs weekday.  


```r
act_clean$dow <- weekdays(act_clean$date)
act_clean$we_flag <- (act_clean$dow %in% c("Sunday", "Saturday"))
act_clean$we_fact <- "weekday"
act_clean[act_clean$we_flag,]$we_fact <- "weekend"
act_clean$we_fact <- as.factor(act_clean$we_fact)
str(act_clean)
```

```
## 'data.frame':	17568 obs. of  6 variables:
##  $ steps   : num  1.717 0.3396 0.1321 0.1509 0.0755 ...
##  $ date    : Date, format: "2012-10-01" "2012-10-01" ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
##  $ dow     : chr  "Monday" "Monday" "Monday" "Monday" ...
##  $ we_flag : logi  FALSE FALSE FALSE FALSE FALSE FALSE ...
##  $ we_fact : Factor w/ 2 levels "weekday","weekend": 1 1 1 1 1 1 1 1 1 1 ...
```

Calculate averages weekends vs weekdays


```r
act_weekday <- act_clean[act_clean$we_flag == F,]
act_weekend <- act_clean[act_clean$we_flag == T,]
act_weekday_avg <- aggregate(steps ~ interval, data = act_weekday, mean)
act_weekend_avg <- aggregate(steps ~ interval, data = act_weekend, mean)
head(act_weekday_avg)
```

```
##   interval      steps
## 1        0 2.25115304
## 2        5 0.44528302
## 3       10 0.17316562
## 4       15 0.19790356
## 5       20 0.09895178
## 6       25 1.59035639
```

```r
head(act_weekend_avg)
```

```
##   interval       steps
## 1        0 0.214622642
## 2        5 0.042452830
## 3       10 0.016509434
## 4       15 0.018867925
## 5       20 0.009433962
## 6       25 3.511792453
```

Draw panel plots


```r
par(mfrow=c(1,2))
plot(act_weekend_avg$interval, act_weekend_avg$steps
     , type = "l"
     , col = "green"
     , xlab = "Interval"
     , ylab = "Steps"
     , main = "Steps per interval (weekends)"
     )
plot(act_weekday_avg$interval, act_weekday_avg$steps
     , type = "l"
     , col = "red"
     , xlab = "Interval"
     , ylab = "Steps"
     , main = "Steps per interval (weekdays)"
     )
```

![](PA1_template_files/figure-html/we_avg_plots-1.png)<!-- -->
