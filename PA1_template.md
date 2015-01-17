---
output: html_document
---

Reproducible Research 
Peer Assignment 1  
Sunday, January 18, 2015
  
  
  
  
  
============  
Introduction  
============  

This assignment makes use of data from a personal activity monitoring device. The device collected data at 5 minute intervals throughout the day. The data consists of two months of data from an anonymous person collected during the months of October and November 2012 and include the number of steps taken in 5 minute intervals each day.
  
  
  
  
  
==============================  
Assignment 1a  
Loading and Preprocessing Data  
==============================  

For this assignment, data were downloaded from 

"https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip". The data were downloaded from the site on November 13, 2014 at 12:15:52 PM PST (time zone: San Francisco CA USA).

The data file was unzipped into the C:/Users/judson/Documents/R/dataScienceCoursera/5. ReproducibleResearch/ "local folder". The unzipped file is activity.csv.

This file was loaded into R as such:


```r
activity <- read.csv("C:\\Users\\judson\\Documents\\R\\dataScienceCoursera\\5. ReproducibleResearch\\activity.csv")
dim(activity)
```

```
## [1] 17568     3
```

```r
names(activity)
```

```
## [1] "steps"    "date"     "interval"
```

The basic data set was not further processed, although other datasets for various analyses were derived from it.

Exploratory data analyses were performed to facilitate a basic understanding of the data (these analyses are not all shown but some basic facts about the data are given below).

There are 17,568 observations each consisting of the variables steps, date, and interval. The read.csv function loaded steps and interval as integers. It loaded date as a factor with 61 levels, one for each day between 2012-10-01 through 2012-11-30 (yyyy-mm-dd format). Data used for analyses below in some cases (identified as such) have been converted to a "date" class or a simple integer sequence of days to facilitate plotting and time series analysis but the basic dataset variables were not converted or transformed.

The intervals range from 0 to 2355 in 5 minute lengths and represent the staring minute (and hour) of the intervals without leading 0s (i.e., 0 is the 5 minute interval starting at 12:00am; that is 0:00, 55 is the last interval in the midnight hour, 2355 is the 5 minute interval just before noon, etc.). There are, therefore, 12 5-minute intervals per hour for each 24-hour day and each date has the full complement of interval records. Hence for each day, there are 12 x 24 = 288 intervals. There are 61 days in the period so the simple multiplication of 61 x 288 = 17,568 accounts for all of the expected and extant records.

In much of the remaining analyses, we will want to use functions from the dplyr and lubridate packages.


```r
library(dplyr)
library(lubridate)
```
  
  
  
  
  
============================================  
Assignment 1b  
Determine the mean total steps taken per day  
============================================

To apply the mean function to these data (by date) we will have to remove or suppress NA values in the steps variable. First we account for the NAs.


```r
summary(activity$steps)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
##    0.00    0.00    0.00   37.38   12.00  806.00    2304
```

```r
as.character(unique(activity$date[is.na(activity$steps)]))
```

```
## [1] "2012-10-01" "2012-10-08" "2012-11-01" "2012-11-04" "2012-11-09"
## [6] "2012-11-10" "2012-11-14" "2012-11-30"
```

```r
activityCC <- activity[complete.cases(activity),]
dim(activityCC)
```

```
## [1] 15264     3
```

We find that there are 2,304 records in which the steps variable is missing (NA). A bit of analysis shows that these NAs all occur on one of the 8 dates shown. The multiplication 8 x 288 = 2,304 accounts for all of the NAs in the steps variable. A curiosity is that the NA "dates" are 2 Mondays, 2 Fridays, and 1 Wednesday, Thursday, Saturday, and Sunday. There is no explanation as to why NAs fall into exact, contiguous 24 hour periods and why they fall on these days of the week. This would be perhaps helpful to know but does not appear to be problematic. We use the dataframe *activityCC*, denoting that it is derived from the original dataframe but contains only the complete cases.

When we assign the complete cases of activity to the working file *activityCC*, we find that exactly the 2,304 expected cases were removed. *activityCC* has 15,264 records and the same 3 variables.

Let us produce a histogram of the total number of steps taken each day...


```r
by_dateCC <- group_by(activityCC, date)
totStepsByDate <- summarise(by_dateCC, stepSum = sum(steps))
stepSum <- as.numeric(totStepsByDate$stepSum)
par(mar=c(4,4,4,4))
hist(stepSum, breaks = c(0, 4000, 8000, 12000, 16000, 20000, 24000), xlim=c(0,25000), ylim=c(0,25), xlab="Count of Steps", ylab="Frequency in 53 of the 61 Days", main="Total Steps per Day")
```

![plot of chunk totStepsByDate](figure/totStepsByDate-1.png) 

...and find the mean and median values among the complete cases.


```r
mean(stepSum)
```

```
## [1] 10766.19
```

```r
median(stepSum)
```

```
## [1] 10765
```

```r
rm(by_dateCC, totStepsByDate, stepSum)
```

The required deliverables for Assignment 1b can be read in the output above.
  
  
  
  
  
============================================  
Assignment 1c  
Determine the average daily activity pattern  
============================================  

Prepare to make a time series plot of mean steps by interval (across all days).


```r
        steps <- as.numeric(activityCC$steps)
        activityCC$steps <- steps

        ## Note: 288 is the number of 5 minute intervals in a day.

        activityCCTS <- cbind(activityCC, intOrd = rep(1:288, times =53))
        stepsTimeSeries <- activityCCTS[,c(4,1)]
                steps_byInt <- group_by(stepsTimeSeries, intOrd)
                meanStepsByInt <- summarize(steps_byInt, meanSteps = mean(steps))
        dim(meanStepsByInt)
```

```
## [1] 288   2
```
  

We have the correct dimensions; let us create the time series plot.


```r
plot(meanSteps ~ intOrd, data=meanStepsByInt, type="l", xlab = "Ordinal Position of 5-minute Interval During 24-hour Day", ylab = "Mean Step Within Interval Across All Complete Case Days")
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-1.png) 

Which interval across all days contains the highest average number of steps?


```r
which.max(meanStepsByInt$meanSteps)
```

```
## [1] 104
```

The maximum occurs in the 104th interval, or beginning at 8:35am.
  
  
  
  
  
====================================  
Assignment 1d  
Impute missing values and re-analyze  
==================================== 

We had previously identified the incomplete cases. All of the missing values are in the *steps* variable. There are 2,304 of them. 

Let us split the data base into complete and incomplete cases, remove the NAs, replacing them with the average steps by interval and recombine. Check that the new database is of the right size.


```r
activityCC <- activity[complete.cases(activity),]
activityIC <- activity[!complete.cases(activity),]
x <- unlist(meanStepsByInt[,2])
replace <- c(x, x, x, x, x, x, x, x)
activityIC$steps <- replace
activityRebuilt <- rbind(activityCC, activityIC)
dim(activityRebuilt)
```

```
## [1] 17568     3
```

```r
summary(activityRebuilt)
```

```
##      steps                date          interval     
##  Min.   :  0.00   2012-10-01:  288   Min.   :   0.0  
##  1st Qu.:  0.00   2012-10-02:  288   1st Qu.: 588.8  
##  Median :  0.00   2012-10-03:  288   Median :1177.5  
##  Mean   : 37.38   2012-10-04:  288   Mean   :1177.5  
##  3rd Qu.: 27.00   2012-10-05:  288   3rd Qu.:1766.2  
##  Max.   :806.00   2012-10-06:  288   Max.   :2355.0  
##                   (Other)   :15840
```

```r
sum(complete.cases(activityRebuilt))
```

```
## [1] 17568
```

```r
rm(x, replace, activityIC, activityCC)
```

With missing values replaced, we recompute the mean and median statistics, using the name addition *AC* to indicate all cases, including those with imputed values.


```r
by_dateAC <- group_by(activityRebuilt, date)
totalStepsByDateAC <- summarise(by_dateAC, stepSum=sum(steps))
stepSum <- as.numeric(totalStepsByDateAC$stepSum)
mean(stepSum)
```

```
## [1] 10766.19
```

```r
median(stepSum)
```

```
## [1] 10766.19
```
The mean and median values are identical, but more importantly, they are very close to the values computed by  removing missing values - as expected - we have used the means for substitute values.
  
  
  
  
  
==============================================================  
Assignment 1e
Differences in activity patterns between weekdays and weekends  
==============================================================

We create a new dataframe with a weekday/weekend factor and add the serial 5-minute interval for smooth plotting in a time series (i.e. interval = 0 5...2355 -> intOrd 0 1...288). Because the dataframe *activityRebuilt* is very regularly laid out, this will require little to no calculation. Our final working dataframe will be called *activityFinal*.


```r
x <- "weekday"
y <- "weekend"
xDay <- rep(x, times=288)
yDay <- rep(y, times=288)
dayPattern <- c(xDay, xDay, xDay, xDay, xDay, yDay, yDay)
dayPattern <- rep(dayPattern, times=9)
dayPattern <- dayPattern[1:17568]
timeIntOrd <- rep(1:288, times=61)
activityFinal <- cbind(activityRebuilt, dayPattern, timeIntOrd)
dim(activityFinal)
```

```
## [1] 17568     5
```

```r
names(activityFinal)
```

```
## [1] "steps"      "date"       "interval"   "dayPattern" "timeIntOrd"
```

The final basic dataframe has been built. For convenience, we will separate into a weekday dataframe and a weekend dataframe. We will take mean steps per time interval for the two separate dataframes and plot them for comparison.


```r
activityFinalWday <- activityFinal[dayPattern=="weekday",]
activityFinalWend <- activityFinal[dayPattern=="weekend",]
steps_byIntWday <- group_by(activityFinalWday, timeIntOrd)
steps_byIntWend <- group_by(activityFinalWend, timeIntOrd)
meanStep_byIntWday <- summarise(steps_byIntWday, meanSteps=mean(steps))
meanStep_byIntWend <- summarise(steps_byIntWend, meanSteps=mean(steps))
par(mfrow=c(1,2))
plot(meanSteps ~ timeIntOrd, data=meanStep_byIntWday, type="l", xlab = "Seq. of 5-minute Intervals in 24 Hours", ylab = "Mean Steps on Weekdays", ylim = c(0,250))
plot(meanSteps ~ timeIntOrd, data=meanStep_byIntWend, type="l", xlab = "Seq. of 5-minute Intervals in 24 Hours", ylab = "Mean Steps on Weekends", ylim = c(0,250))
```

![plot of chunk unnamed-chunk-9](figure/unnamed-chunk-9-1.png) 

The side-by-side time series plots show similarity in levels of activity for weekdays and weekends. Reasons for this may be that

        . averages by interval (without regard for day) were used for missing data.
        . the missing data has a curious pattern that is hard to explain.
        . data were collected on one individual over a short period and may not reflect population wide trends.
        . any number of other factors such as equipment failure, forgetfulness to use. 
        . downtime in recording equipment, unusual period in the individual's life, etc.
        
The peaks and valleys of the weekday activity appear more pronounced, suggesting that activity is generated by practical necessity during the workday. Activity is slightly higher overall on weekends, but the timing of activities involving walking appears quite correlated.




