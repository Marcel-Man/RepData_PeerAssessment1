---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---


## Loading and preprocessing the data

Here we first unzip and load the csv file into R. Then, we convert the date and interval into one single datetime variable for processing. This is achieved by first padding zeros to the interval, then concatenating it with the date, and parsed with ymd_hm function from lubridate library. Finally, we form the processed dataframe by taking the datetime formatted and steps variable.


```r
library(lubridate)
```

```
## 
## Attaching package: 'lubridate'
```

```
## The following objects are masked from 'package:base':
## 
##     date, intersect, setdiff, union
```

```r
library(stringr)
library(dplyr)
```

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

```r
library(ggplot2)
Sys.setenv("LANGUAGE"="En")
Sys.setlocale("LC_ALL", "English")
```

```
## [1] "LC_COLLATE=English_United States.1252;LC_CTYPE=English_United States.1252;LC_MONETARY=English_United States.1252;LC_NUMERIC=C;LC_TIME=English_United States.1252"
```

```r
unzip("activity.zip")
act <- read.csv("activity.csv")
act_df <- data.frame(datetime=ymd_hm(paste(act$date, str_pad(act$interval, 4, side="left", pad="0"))), steps=act$steps)
```

## What is mean total number of steps taken per day?

Here, we calculate the total number of steps per day by grouping the date in our datetime variable. Below we show the distribution by plotting the histogram. Then we calculate the mean and median of the number of steps per day.


```r
steps_by_day <- act[!is.na(act$steps),] %>% group_by(date) %>% summarize(sum=sum(steps, na.rm=TRUE))
```

```
## `summarise()` ungrouping output (override with `.groups` argument)
```

```r
hist(steps_by_day$sum, breaks=50, main="Daily number of steps", xlab="Number of steps", ylab="Frequency")
```

![](PA1_template_files/figure-html/unnamed-chunk-2-1.png)<!-- -->

```r
mean(steps_by_day$sum)
```

```
## [1] 10766.19
```

```r
median(steps_by_day$sum)
```

```
## [1] 10765
```

## What is the average daily activity pattern?

Next, we compute the average number of steps over each 5 mins interval and plot the resulting time series. The interval at which the maximum average number of steps is recorded is also calculated.


```r
steps_by_interval <- act[!is.na(act$steps),] %>% group_by(interval) %>% summarize(mean=mean(steps, na.rm=TRUE))
```

```
## `summarise()` ungrouping output (override with `.groups` argument)
```

```r
with(steps_by_interval, plot(interval, mean, type="l", xlab="Interval", ylab="Average Number of Steps", main="Average number of steps in one day"))
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

```r
steps_by_interval[which(steps_by_interval$mean==max(steps_by_interval$mean)),]
```

```
## # A tibble: 1 x 2
##   interval  mean
##      <int> <dbl>
## 1      835  206.
```

## Imputing missing values

Notice that the original data contains a number of NAs in the steps information. The number of such data is shown below. To impute the missing data, we calculate the number of steps measured during the same hour in the same day of the missing data. After imputing the data, we then plotted the histogram and calculated the mean and median of the new data set. The new mean and median has moved to the left as compared to before, since the imputed values are mostly zero.


```r
sum(is.na(act$steps))
```

```
## [1] 2304
```

```r
steps_by_hour <- act %>% group_by(date, hour=interval %/% 100) %>% summarize(sum=sum(steps, na.rm=TRUE))
```

```
## `summarise()` regrouping output by 'date' (override with `.groups` argument)
```

```r
act_imputed <- act %>% mutate(hour=interval%/%100) %>% left_join(steps_by_hour, by=c("date","hour")) %>% mutate(steps=ifelse(is.na(steps), sum, steps)) %>% select(c("steps","date","interval"))
steps_by_day <- act_imputed %>% group_by(date) %>% summarize(sum=sum(steps))
```

```
## `summarise()` ungrouping output (override with `.groups` argument)
```

```r
hist(steps_by_day$sum, breaks=50, main="Daily number of steps", xlab="Number of steps", ylab="Frequency")
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

```r
mean(steps_by_day$sum)
```

```
## [1] 9354.23
```

```r
median(steps_by_day$sum)
```

```
## [1] 10395
```

## Are there differences in activity patterns between weekdays and weekends?

Finally, we compare the activity patterns between weekdays and weekends by comparing them on 2 panels on a plot. 


```r
steps_by_wday_interval <- act_imputed %>% mutate(wday=as.factor(ifelse(grepl("S(at|un)", weekdays(ymd(date), abbr=TRUE)), "weekend", "weekday"))) %>% group_by(wday, interval) %>% summarize(mean=mean(steps))
```

```
## `summarise()` regrouping output by 'wday' (override with `.groups` argument)
```

```r
ggplot(steps_by_wday_interval, aes(interval, mean)) + geom_line() + facet_wrap(.~wday, dir="v") + labs(title = "Average number of steps on weekdays/weekends", x="Interval", y="Mean number of steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png)<!-- -->

  
