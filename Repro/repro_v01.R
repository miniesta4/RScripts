repro <- function(){
    
    ## Load libraries
    library(dplyr)
    library(ggplot2)
    
    ## Load and preprocess data
    unzip ("./activity.zip")
    file <- read.csv("./activity.csv")
    ds1 <- file %>% mutate(date = as.Date(date))
    
    ## Number of steps taken per day
    ds2 <- ds1 %>% group_by(date) %>% summarize(freq = sum(steps, na.rm = TRUE))
    
    qplot(date, freq, data = ds2, geom = "histogram", stat = "identity",
          ylab = "Steps", main = "Number of steps taken each day")
    
    ds3 <- ds2 %>% summarize(avg = mean(freq, na.rm = TRUE), med = median(freq, na.rm = TRUE))
    
    ## Average daily activity pattern
    ds4 <- ds1 %>% group_by(interval) %>% summarize(avg = mean(steps, na.rm = TRUE))
    
    qplot(interval, avg, data = ds4, geom = "line", ylab = "Steps",
          main = "Average number of steps taken")
    
    ds4 %>% filter(avg == max(ds4$avg))
    
    ## Imputing missing values
    ## Missing values in the dataset
    sum(is.na(ds1$steps))
    
    ## Median for that 5-minute interval
    ds5 <- ds1 %>% group_by(interval) %>% summarize(med = median(steps, na.rm = TRUE))
    
    ## Missing data filled in.
    ds6 <- ds1 %>% group_by(interval) %>% mutate(med = median(steps, na.rm = TRUE))
    ds7 <- ds6 %>% mutate(steps_med = ifelse(is.na(steps), med, steps))
    
    ## Number of steps taken per day
    ds8 <- ds7 %>% group_by(date) %>% summarize(freq = sum(steps_med))
    
    qplot(date, freq, data = ds8, geom = "histogram", stat = "identity",
          ylab = "Steps", main = "Number of steps taken each day")
    
    ds9 <- ds8 %>% summarize(avg = mean(freq), med = median(freq))
    
    ## Patterns between weekdays and weekends
    ## Set locale
    Sys.setlocale("LC_TIME", locale = "en_US.UTF-8")
    
    ds10 <- ds7 %>% mutate(weekday = weekdays(date))
    
    ds11 <- ds10 %>% mutate(wd = as.factor(
        ifelse(weekday %in% c("Saturday", "Sunday"), "weekend", "weekday")))
    
    ds12 <- ds11 %>% group_by(interval, wd) %>% summarize(avg = mean(steps_med))
    
    qplot(interval, avg, data = ds12, facets = wd ~ ., geom = "line", ylab = "Steps",
          main = "Average number of steps taken")
}

