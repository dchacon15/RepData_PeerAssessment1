# Reproducible Research: Peer Assessment 1

This is the documentation report for the Reproducible Research course Peer Assessment 1. This assignment makes use of data from a personal activity monitoring device. This device collects data at 5 minute intervals through out   the day.

## Loading and preprocessing the data

To begin the research, the data from the monitoring devices needs to be loaded and the the date values must be changed to "Date" data type to make more easy the following analysis.

```r
if(!file.exists("activity.csv")){
    unzip("activity.zip",exdir = ".")
}
activityData <- read.csv("activity.csv",na.strings = "NA")
activityData$date <- as.Date(activityData$date)
```

## What is mean total number of steps taken per day?

For this question the data must be aggregated to calculate the total number of steps per day

```r
totalStepsPerDay <- aggregate(steps~date, activityData, sum)
```

Then, a histogram must be created to view frequency of the total number of steps taken each day

```r
library(ggplot2)
ggplot(totalStepsPerDay,aes(steps))+
    geom_histogram(bins = 20, fill = "turquoise") +
    ggtitle("Number of steps taken each day \nduring the months of October and November, 2012")+
    xlab("Number of steps taken each day")
```

<img src="PA1_template_files/figure-html/HistogramSteps-1.png" style="display: block; margin: auto;" />

Finally, the mean and the median of total number of steps is calculated and printed.

```r
mean(totalStepsPerDay$steps)
```

```
## [1] 10766.19
```

```r
median(totalStepsPerDay$steps)
```

```
## [1] 10765
```


## What is the average daily activity pattern?

For this questions,a time lapse (trend) plot to view the daily activity pattern and the changes between every 5 minute interval. 

```r
averageNumberSteps <- aggregate(steps~interval, activityData, mean)

ggplot(averageNumberSteps,aes(interval,steps))+
    geom_line(col="dodgerblue") + 
    labs(title = "Average Number of Steps taken in 5 minutes intervals \nduring the months of October and November, 2012")+
    xlab("5-min Interval")+
    ylab("Avg. Steps Across All Days")
```

<img src="PA1_template_files/figure-html/TrendPlot-1.png" style="display: block; margin: auto;" />


With this information, a query can be build to view the 5 minute intercal that contains the maximum number of steps

```r
subset(averageNumberSteps,steps == max(averageNumberSteps$steps,na.rm = TRUE))
```

```
##     interval    steps
## 104      835 206.1698
```


## Imputing missing values

It's important to know how many NA values are in our data because this missing values can affect the analysis.

```r
sum(is.na(activityData$steps))
```

```
## [1] 2304
```

To use this information without filtering the data from the dataset, the missing values can be filled with a strategy. In this case the strategy used is calculating the 5 minute interval.

```r
filledMissingNA <- merge(activityData,averageNumberSteps, by = "interval")
filledMissingNA$steps <- ifelse(is.na(filledMissingNA$steps.x),
                                filledMissingNA$steps.y,
                                filledMissingNA$steps.x)
filledMissingNA <- subset(filledMissingNA, select = c(interval,steps,date))
```

With this new data set we can view and compare the the results with the previous calculated data. We need to make histogram and make the same mean and median calculations so we can answer the following questions:
<br> 1. Do these values differ from the estimates from the first part of the assignment?
<br> 2. What is the impact of imputing missing data on the estimates of the total daily number of steps?

```r
totalStepsPerDayNoNA <- aggregate(steps~date, filledMissingNA, sum)
ggplot(totalStepsPerDayNoNA,aes(steps))+
    geom_histogram(bins = 20, fill = "indianred1") +
    ggtitle("Number of steps taken each day \nduring the months of October and November, 2012")+
    xlab("Number of steps taken each day")
```

<img src="PA1_template_files/figure-html/filledHistogram-1.png" style="display: block; margin: auto;" />

```r
mean(totalStepsPerDayNoNA$steps)
```

```
## [1] 10766.19
```

```r
median(totalStepsPerDayNoNA$steps)
```

```
## [1] 10766.19
```

With this results the information with NA's are a little bit different from the previously calculated. The mean in this case is the same but the median changes a little bit.

## Are there differences in activity patterns between weekdays and weekends?

We can see if the information differs if it's on a weekend or a weekday, so it's needed to create a factor variable to define if a day is weekend or not.

```r
filledMissingNA$DateType <- ifelse(weekdays(filledMissingNA$date)=="Sunday"|
                                       weekdays(filledMissingNA$date)=="Saturday",
                                   "Weekend","Weekday")
filledMissingNA$DateType <- as.factor(filledMissingNA$DateType )
```

Then, creating a time series plot with two panels we can se the difference.

```r
averageNumberStepsNoNA <- aggregate(steps~interval+DateType, filledMissingNA, mean)
qplot(interval,steps,data=averageNumberStepsNoNA,facets = DateType~.,geom = "line",
      col=DateType, main ="Average Number of Steps taken in 5 minutes intervals \nduring the months of October and November, 2012 by date type")
```

<img src="PA1_template_files/figure-html/PanelPlot-1.png" style="display: block; margin: auto;" />

With this information we can see that there is an important difference between the weekdays and the weekends.
