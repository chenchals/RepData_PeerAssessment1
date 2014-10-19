# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data

```r
df<- read.csv(unz("repdata-data-activity.zip", "activity.csv"), header=TRUE ,sep=",")
df$DateField<-as.Date(df$date)
df$Weekday<-weekdays(df$DateField)
```

## What is mean total number of steps taken per day?
Shown below is the histogram of the total number of steps taken per day.

```r
library(lattice)
totalStepsByDate<-aggregate(df$steps, by=list(df$date),FUN=sum)
names(totalStepsByDate)<-c("Date","Steps")
totalStepsPerDayPlot<-barchart(Steps ~ Date, data=totalStepsByDate,
            main="Total Number of Steps taken Daily",
            ylab="Total number of steps", xlab="Date", 
            scales = list(y=list(tick.number=6), 
                          x=list(cex=0.6,tick.number=7,rot=80)
                          )
            )

print(totalStepsPerDayPlot)
```

![](./PA1_template_files/figure-html/createPlotSums-1.png) 




```r
meanPerDay<-mean(totalStepsByDate$Steps,na.rm = TRUE)
medianPerDay<-median(totalStepsByDate$Steps,na.rm = TRUE)
```
The daily **mean** total number of the steps taken per day are **10766** and the **median** total nuber of steps taken per day are **10765**.

## What is the average daily activity pattern?



## Imputing missing values



## Are there differences in activity patterns between weekdays and weekends?