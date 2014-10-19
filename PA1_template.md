# Reproducible Research: Peer Assessment 1


### Loading and preprocessing the data

```r
df<- read.csv(unz("repdata-data-activity.zip", "activity.csv"), header=TRUE ,sep=",")
df$DateField<-as.Date(df$date)
df$Weekday<-weekdays(df$DateField)
```

### What is mean total number of steps taken per day?


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

**Figure 1.** *Histogram of total number of steps taken in a day for a single individual over the period of study. Number steps in 5 minute intervals were collected for each day over 61 days.*



```r
meanPerDay<-mean(totalStepsByDate$Steps,na.rm = TRUE)
medianPerDay<-median(totalStepsByDate$Steps,na.rm = TRUE)
```
The daily **mean** total number of the steps taken per day is **10766** and the **median** total number of steps taken per day is **10765**.  *Missing values were excluded in computing the mean and median.*

## What is the average daily activity pattern?



2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?


```r
library(lattice)
avgActivity<-aggregate(df$steps, by=list(df$interval),FUN=mean, na.rm=TRUE)
names(avgActivity)<-c("Interval","Activity")
yMax.y<-max(avgActivity$Activity)
yMax.x<-avgActivity$Interval[which.max(avgActivity$Activity)]
yMax.time<-strftime( as.POSIXct( "1970-01-01" ) + as.difftime( yMax.x, units="mins" ), "%I:%M %p" )
avgActivityOver5Mins<-xyplot(Activity ~ Interval, data=avgActivity, type="l", 
       main="Average activity over 5 minute intervals\n (Across all days)", 
       xlab="Minutes", ylab="Average Activity (Number of Steps)",
       panel=function(x,y,...){
         panel.xyplot(x,y,...)
         panel.abline(v =avgActivity$Interval[which.max(avgActivity$Activity)],col = "red", lwd = 2)
         panel.text(x=yMax.x+700,y=yMax.y,labels=paste(c("Interval:",yMax.x," (",yMax.time,")"),sep="", collapse=""))
         panel.text(x=yMax.x+700,y=yMax.y-15,labels=paste(c("Activity:",format(yMax.y,digits=4)),sep="", collapse=""))
         })
print(avgActivityOver5Mins)
```

![](./PA1_template_files/figure-html/createPlotMeans-1.png) 


**Figure 2.** *Time series plot of the average number of steps taken during 5-minute intervals across all days. The plot show higher activity during the 800 to 930 minutes (1:20 PM to 3:15 PM), possibly due to vigorous exercise. (b) The 5-minute interval corresponding to 835 minutes (around 01:55 PM) had an average maximum of 206 steps.*

### Imputing missing values

### see: http://www.stat.columbia.edu/~gelman/arm/missing.pdf
### random.imp <- function (a){
### missing <- is.na(a)
### n.missing <- sum(missing)
### a.obs <- a[!missing]
##### imputed <- a
##### imputed[missing] <- sample (a.obs, n.missing, replace=TRUE)
##### return (imputed)
##### }
###### (To see how this function works, take a small dataset and evaluate the function line
###### by line.) We use random.imp to create a completed data vector of earnings:
###### R code earnings.imp <- random.imp (earnings)
###### visualizing missing values marginplot(
###### http://cran.r-project.org/web/packages/mice/mice.pdf
###### http://teachpress.environmentalinformatics-marburg.de/2013/07/creating-publication-quality-graphs-in-r-7/





## Are there differences in activity patterns between weekdays and weekends?
