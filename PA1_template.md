# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data

Unzip the file and read the csv file "activity.csv"

```r
library(dplyr)
```

```
## 
## Attaching package: 'dplyr'
## 
## The following object is masked from 'package:stats':
## 
##     filter
## 
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

```r
unzip(zipfile = "activity.zip")
activity <- read.csv("activity.csv", header = TRUE , stringsAsFactors = FALSE )
```

Make the date culumn into a date type 

```r
activity[,2] <- as.Date(activity[,2])
```



## What is mean total number of steps taken per day?


```r
gb <- group_by( activity[complete.cases(activity),] , date)
TotalSteps <- summarise(gb,total = sum(steps))
hist(TotalSteps$total, main="Total Steps Per Day Density",xlab = "Total Steps" )
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png) 

```r
meanSteps <- mean(TotalSteps$total)
medianSteps <- median(TotalSteps$total)
```

For Total Steps per Day Density the mean is 10766.2 and the median is 10765

## What is the average daily activity pattern?


```r
gbi <- group_by( activity[complete.cases(activity),] , interval)
avgi <- summarise(gbi,avg = mean(steps))
maxInterval <- filter(avgi, avg == max(avgi$avg) )
plot(avgi,type = "l")
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png) 
The Interval that contains the maximum number of steps (on average) is: 835

## Imputing missing values


```r
nas <- complete.cases(activity)
tb <- table(nas)
```

The number of rows with NA's are: 2304

We will make a copy of the data and fill in NA vales 

```r
activity_no_na <- activity
```

We will replace NA's with the average steps for that interval

```r
activity_no_na[is.na(activity_no_na),1] <- avgi[ avgi$interval == activity_no_na[is.na(activity_no_na),3],2]
```

Plot the Histogram and calculate the mean and median

```r
TotalSteps <- summarise(gb,total = sum(steps))
hist(TotalSteps$total, main="Total Steps Per Day Density (no NA's)",xlab = "Total Steps" )
```

![](PA1_template_files/figure-html/unnamed-chunk-8-1.png) 

```r
meanSteps <- mean(TotalSteps$total)
medianSteps <- median(TotalSteps$total)
```

For Total Steps per Day Density the mean is 10766.2 and the median is 10765

We see that there is no diferance to the mean or the median when filling in NA's with coresponding interval mean.


## Are there differences in activity patterns between weekdays and weekends?

Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.


```r
activity_no_na$wk <- factor( (weekdays(as.Date(activity_no_na[,2] )) %in% c("Sunday","Saturday") ), labels = c("weekday","weekend"))
```

Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). 

```r
par(mfrow = c(2, 1),mar=c(4,4,2,1))
plot(avgi,type = "l")

gbi_na <- group_by( activity_no_na[complete.cases(activity_no_na ),] , interval)
avgi_na <- summarise(gbi_na,avg = mean(steps))
maxInterval_na <- filter(avgi_na, avg == max(avgi_na$avg) )
plot(avgi_na,type = "l")
```

![](PA1_template_files/figure-html/unnamed-chunk-10-1.png) 
