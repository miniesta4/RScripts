## v02

repro <- function(){

    ## Loading and preprocessing the data
    file <- read.csv("./activity.csv")
    ds1 <- transform(file, date = as.Date(date))
    
    ## What is mean total number of steps taken per day?
    ds2 <- aggregate(steps ~ date, data = ds1, sum)
    qplot(ds2$steps, data = ds2, main = "Histogram of steps taken each day")
    mean(ds2$steps)
    median(ds2$steps)

    ##What is the average daily activity pattern?
    ds4 <- aggregate (steps ~ interval, data = ds1, mean)
    qplot(interval, steps, data = ds4, geom = "line", ylab = "Steps",
          main = "Average number of steps taken")
    ds4[ds4$steps == max(ds4$steps), ]
    
    
    ## Imputing missing values
    sum(is.na(ds1$steps))
    aggregate(steps ~ date, data = ds1[is.na(ds1$steps), ], length, na.action = "na.pass")
    
    ds5 <- aggregate(steps ~ interval, data = ds1, median)
    qplot(interval, steps, data= ds5, geom = "line", ylab = "Steps",
          main = "Median 5-minute interval")
    
    ds6 <- merge(ds1, ds5, by.x = "interval", by.y = "interval")
    ds7 <- transform(ds6, steps.med = ifelse(is.na(steps.x), steps.y, steps.x))
    ds8 <- aggregate(steps.med ~ date, data = ds7, sum)
    qplot(steps.med, data = ds8, main = "Histogram of steps taken each day")
    mean(ds8$steps)
    median(ds8$steps)
    
}