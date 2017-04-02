# Reproducible Research: Peer Assessment 1

## Loading and preprocessing the data

```r
if(!file.exists(file.path("rawdata","activity.csv"))) {
        unzip(file.path("activity.zip"), overwrite = FALSE, exdir = "rawdata")
    }
```




## What is mean total number of steps taken per day?



## What is the average daily activity pattern?



## Imputing missing values



## Are there differences in activity patterns between weekdays and weekends?
