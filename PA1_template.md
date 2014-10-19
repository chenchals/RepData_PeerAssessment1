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

1. Make a time series plot (i.e. `type = "l"`) of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?


```r
library(lattice)
avgActivity<-aggregate(df$steps, by=list(df$interval),FUN=mean, na.rm=TRUE)
names(avgActivity)<-c("Interval","Activity")
yMax.y<-max(avgActivity$Activity)
yMax.x<-avgActivity$Interval[which.max(avgActivity$Activity)]
avgActivitOver5Mins<-xyplot(Activity ~ Interval, data=avgActivity, type="l", 
       main="Average activity over 5 minute intervals\n (Across all days)", 
       xlab="Minutes", ylab="Average Activity (Number of Steps)",
       panel=function(x,y,...){
         panel.xyplot(x,y,...)
         panel.abline(v =avgActivity$Interval[which.max(avgActivity$Activity)],col = "red", lwd = 2)
         panel.text(x=yMax.x+700,y=yMax.y,labels=paste(c("Interval:",yMax.x),sep="", collapse=""))
         panel.text(x=yMax.x+700,y=yMax.y-15,labels=paste(c("Activity:",format(yMax.y,digits=4)),sep="", collapse=""))
         })
print(avgActivitOver5Mins)
```

![](./PA1_template_files/figure-html/createPlotMeans-1.png) 



## Imputing missing values



## Are there differences in activity patterns between weekdays and weekends?
