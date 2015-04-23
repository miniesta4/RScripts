stormResearch <- function(){

## Download file
if !file.exists ("./files/StormData.bz2"){
    fileUrl <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2"
    download.file(fileUrl, destfile = "./files/StormData.bz2", method = "wget")
    dateDownloaded <- date()  # Date downloaded
}

## Read file
if (!ds) {
    
    ##  data5r <- read.csv("./files/StormData.bz2", stringsAsFactors = FALSE,
    na.strings = "", nrow = 5)
##  classes <- sapply(data5r, class)
ds <- read.csv("./files/StormData.bz2", stringsAsFactors = FALSE, 
               na.strings = "")
}

## Data summary
dim(ds)
summary(ds)
names(ds)
sort(round(sapply(ds, function(x) mean(is.na(x))), digits = 2), decreasing = TRUE)

head(ds$EVTYPE)
ag <- aggregate(cbind(FATALITIES, INJURIES) ~ EVTYPE, ds = ds, sum)
ag[order(ag$FATALITIES, ag$INJURIES, decreasing = TRUE), ]

ag[ag$FATALITIES > 0 | ag$INJURIES > 0, ]

}