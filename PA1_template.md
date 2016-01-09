# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data


```r
activity <- read.csv("activity.csv")
```

## What is mean total number of steps taken per day?

First we need to know the total number of days each day.

```r
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
daily.totals <- activity %>%
        group_by(date)%>%
        summarize(total=sum(steps,na.rm=TRUE))
daily.mean <- with(daily.totals,mean(total))
daily.median <- with(daily.totals,median(total))
print(daily.mean)
```

```
## [1] 9354.23
```

```r
print(daily.median)
```

```
## [1] 10395
```
The mean total number of steps taken each day is 9354.23 steps per day. The median is 10395 steps per day.


```r
hist(daily.totals$total,xlab="Daily total steps",main="")
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png) 

## What is the average daily activity pattern?



## Imputing missing values



## Are there differences in activity patterns between weekdays and weekends?
