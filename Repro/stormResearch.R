StormResearch <- function(){}

## Get file

if !file.exists ("./files/StormData.bz2"){
    fileUrl <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2"
    download.file(fileUrl, destfile = "./files/StormData.bz2", method = "wget")
}

dateDownloaded <- date()

if (!data) {
    data5r <- read.csv("./files/StormData.bz2", stringsAsFactors = FALSE, na.strings = "", nrow = 5)
    classes <- sapply(data5r, class)
    data <- read.csv("./files/StormData.bz2", stringsAsFactors = FALSE, na.strings = "", colClasses = classes)
}
dim(data)
summary(data)
names(data)
sapply(data, function(x) mean(is.na(x)))

