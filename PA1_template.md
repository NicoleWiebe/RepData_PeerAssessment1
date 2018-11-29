Loading and Pre-processing the data.
------------------------------------

Set the working directory and read in the CSV file.

``` r
setwd("C:\\Users\\nicole.wiebe\\Desktop\\Coursera")
activity <- read.csv("activity.csv",header=T)
head(activity)
```

    ##   steps       date interval
    ## 1    NA 2012-10-01        0
    ## 2    NA 2012-10-01        5
    ## 3    NA 2012-10-01       10
    ## 4    NA 2012-10-01       15
    ## 5    NA 2012-10-01       20
    ## 6    NA 2012-10-01       25

``` r
str(activity)
```

    ## 'data.frame':    17568 obs. of  3 variables:
    ##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
    ##  $ date    : Factor w/ 61 levels "2012-10-01","2012-10-02",..: 1 1 1 1 1 1 1 1 1 1 ...
    ##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...

``` r
activity$date <- as.Date(activity$date)
```

Changes date to a date instead of a factor.

What is the mean total number of steps taken per day?
-----------------------------------------------------

Ignoring missing values, make a histogram of the total number of steps taken each day and calculate and report the mean and median number of steps per day.

``` r
activity2 <- aggregate(activity$steps, by=list(activity$date), sum)
hist(activity2$x, main = "Histogram of number of steps taken per day")
```

![](PA1_template_files/figure-markdown_github/unnamed-chunk-2-1.png)

``` r
summary(activity2$x, na.rm=T)
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    ##      41    8841   10765   10766   13294   21194       8

The median number of steps per day is 10,765 and the mean is 10,766. You can see that the lowest number of steps per day is 41, which may be an outlier.

What is the average daily activity pattern?
-------------------------------------------

Make a time series plot (i.e. type="l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis). Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

``` r
activity3 <- aggregate(activity$steps, by=list(activity$interval), FUN=mean, na.rm = TRUE)
plot(activity3$Group.1, activity3$x, type="l")
```

![](PA1_template_files/figure-markdown_github/unnamed-chunk-3-1.png)

``` r
which.max(activity3$x)
```

    ## [1] 104

``` r
activity3[104,]
```

    ##     Group.1        x
    ## 104     835 206.1698

From the time series plot, we can see the highest average is somewhere between interval 500-1000. From the code, we can see that the exact interval with the highest average number of steps is interval 835 and the average number of steps is a little over 206.

Imputing missing values
-----------------------

Note that there are a number of days/intervals where there are missing values (coded as NA). The presence of missing days may introduce bias into some calculations or summaries of the data.

Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)

Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

Create a new dataset that is equal to the original dataset but with the missing data filled in.

Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

``` r
library(imputeTS)
library(tidyverse)
```

    ## -- Attaching packages -------------------------------------------- tidyverse 1.2.1 --

    ## v ggplot2 3.1.0     v purrr   0.2.5
    ## v tibble  1.4.2     v dplyr   0.7.7
    ## v tidyr   0.8.2     v stringr 1.3.1
    ## v readr   1.1.1     v forcats 0.3.0

    ## -- Conflicts ----------------------------------------------- tidyverse_conflicts() --
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

``` r
sum(is.na(activity$steps))
```

    ## [1] 2304

``` r
activity$steps2 <- na.mean(activity$steps)
activity4 <- select(activity, steps2, date, interval)
activity4 <- rename(activity4, steps = steps2)
activity5 <- aggregate(activity4$steps, by=list(activity4$date), sum)
hist(activity5$x, main = "Histogram of number of steps taken per day after mean imputation")
```

![](PA1_template_files/figure-markdown_github/unnamed-chunk-4-1.png)

``` r
summary(activity5$x)
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##      41    9819   10766   10766   12811   21194

As we can see, the mean imputation made the data centered a bit more - the mean and median are roughly the same (median is now 10766 instead of 10765), and the bar from 10000 to 15000 on the histogram is a little taller now (data is more symmetrical).

Are there differences in activity patterns between weekdays and weekends?
-------------------------------------------------------------------------

Create a new factor variable in the dataset with two levels -- "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.

Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis).

``` r
weekdays1 <- c('Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday')
activity4$wDay <- factor((weekdays(activity4$date) %in% weekdays1), 
         levels=c(FALSE, TRUE), labels=c('weekend', 'weekday')) 
activity_wkend <- filter(activity4, wDay=='weekend')
activity_wkend2 <- aggregate(activity_wkend$steps, by=list(activity_wkend$interval), FUN=mean, na.rm = TRUE)
activity_wkday <- filter(activity4, wDay=='weekday')
activity_wkday2 <- aggregate(activity_wkday$steps, by=list(activity_wkday$interval), FUN=mean, na.rm = TRUE)
par(mfrow=c(2,1))
plot(activity_wkend2$Group.1, activity_wkend2$x, type="l")
plot(activity_wkday2$Group.1, activity_wkday2$x, type="l")
```

![](PA1_template_files/figure-markdown_github/unnamed-chunk-5-1.png)

The average number of steps is more consistent over the intervals in the weekend, whereas during the week it has a peak.
