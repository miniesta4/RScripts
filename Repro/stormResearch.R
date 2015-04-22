stormResearch <- function(){}

## Get file

if !file.exists ("./files/StormData.bz2"){
    fileUrl <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2"
    download.file(fileUrl, destfile = "./files/StormData.bz2", method = "wget")
}

dateDownloaded <- date()

if (!ds) {
    ds <- read.csv("../files/StormData.bz2", stringsAsFactors = FALSE, na.strings = "")
}
dim(ds)
summary(ds)
names(ds)
colSums(is.na(ds))
round(sapply(ds, function(x) mean(is.na(x))), 2)
ag <- aggregate(cbind(FATALITIES, INJURIES) ~ EVTYPE, data = ds, sum)
ago <- ag[order(ag$FATALITIES, ag$INJURIES, decreasing = TRUE), ]
ago$fat.prop <- round(ago$FATALITIES/sum(ago$FATALITIES), 2)
ago$inj.prop <- round(ago$INJURIES/sum(ago$INJURIES), 2)
agor <- ago[ago$fat.prop >= 0.05 | ago$inj.prop >= 0.05, ]
events <- unique(agor$EVTYPE)
ds_f <- ds[ds$EVTYPE %in% events, c("EVTYPE", "FATALITIES", "INJURIES"), ]
ds_f0 <- ds_f[ds_f$FATALITIES > 0 | ds_f$INJURIES > 0, ]
qplot(log(INJURIES), log(FATALITIES), data = ds_f0, colour = EVTYPE)
