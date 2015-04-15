repro <- function(){
  
  ## Load libraries
  library(dplyr)
  library(ggplot2)
  
  ## Load and preprocess data
  unzip ("./repdata_data_activity.zip")
  file <- read.csv("./activity.csv")
  ds1 <- file %>% mutate(date = as.Date(date))
  
  ## Number of steps taken per day
  ds2 <- ds1 %>% group_by(date) %>% summarize(count = sum(steps, na.rm = TRUE))
  
  qplot(date, count, data = ds2, geom = "histogram", stat = "identity",
        main = "Number of steps taken each day")
  
  ds3 <- ds1 %>% group_by(date) %>% 
    summarize(avg = mean(steps, na.rm = TRUE), med = median(steps, na.rm = TRUE)) %>%
    filter (!is.na(med))
  
  ## Average daily activity pattern
  ds4 <- ds1 %>% group_by(interval) %>% summarize(avg = mean(steps, na.rm = TRUE))
  
  qplot(interval, avg, data = ds4, geom = "line", main = "Average number of steps taken")
  
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
  ds8 <- ds7 %>% group_by(date) %>% summarize(count = sum(steps_med))
  
  qplot(date, count, data = ds8, geom = "histogram", stat = "identity",
        main = "Number of steps taken each day")
  
  ds9 <- ds8 %>% group_by(date) %>% 
    summarize(avg = mean(steps, na.rm = TRUE), med = median(steps, na.rm = TRUE))
}

