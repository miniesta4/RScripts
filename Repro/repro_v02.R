repro <- function(){
  file <- read.csv("./files/activity.csv")
  ds1 <- transform(file, date = as.Date(date))
  
  ds2 <- aggregate(steps ~ date, data = ds1, sum)
  
  qplot(ds2$steps, data = ds2, main = "Histogram of steps taken each day")
  
  mean(ds2$steps)
  
  median(ds2$steps)
  
  ds4 <- aggregate (steps ~ interval, data = ds1, mean)
  
  qplot(interval, avg, data = ds4, geom = "line", ylab = "Steps",
        main = "Average number of steps taken")
  
  sum(is.na(ds1$steps))
  
  df <- ds1[is.na(ds1$steps), ]
  aggregate(steps ~ date, data = ds1[is.na(ds1$steps), ], length, na.action = "na.pass")
}