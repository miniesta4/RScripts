## Weather events analysis

## Load libraries
library(dplyr)
library(ggplot2)

## Download file
if (!file.exists ("./files/StormData.bz2")){
    fileUrl <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2"
    download.file(fileUrl, destfile = "./files/StormData.bz2", method = "wget")
}

dateDownloaded <- date()

## Read file
if (!exists("ds")){
    ds <- read.csv("../files/StormData.bz2", stringsAsFactors = FALSE, na.strings = "")
}
dim(ds)
str(ds)

colSums(is.na(ds))
round(sapply(ds, function(x) mean(is.na(x))), 2)

ds$begin.date <- as.Date(ds$BGN_DATE, "%m/%d/%Y")
summary(ds$begin.date)
summary(ds$begin.date[ds$begin.date > 1995-04-20])
sort(ds$begin.date[ds$begin.date > 1995-04-20])

## EVTYPE cleaning
table(ds$EVTYPE)
unique(ds$EVTYPE)
length(unique(ds$EVTYPE))
str(as.factor(ds$EVTYPE))
summary(as.factor(ds$EVTYPE))



ds$evt <-ds$evt <- toupper (ds$EVTYPE)
ds$evt <- gsub("^ *| *$", "", ds$evt)
ds$evt <- gsub(" {2,}", " ", ds$evt)
ds$evt <- gsub(" /|/ ", "", ds$evt)
ds$evt <- gsub("COLD$", "COLD/WIND CHILL", ds$evt)
ds$evt <- gsub("^FLOOD(.)+", "FLOOD", ds$evt)
ds$evt <- gsub("^HEAT(.)+", "HEAT", ds$evt)
ds$evt <- gsub("^HIGH WIND(.)+", "HIGH WIND", ds$evt)
ds$evt <- gsub("HURRICANE(.)*", "HURRICANE (TYPHOON)", ds$evt)
ds$evt <- gsub("^MARINE TSTM(.)*", "MARINE THUNDERSTORM WIND", ds$evt)
ds$evt <- gsub("^THUNDERSTORM(.)+", "THUNDERSTORM WIND", ds$evt)
ds$evt <- gsub("^TSTM WIND(.)*", "THUNDERSTORM WIND", ds$evt)
ds$evt <- gsub("^URBAN", "FLOOD", ds$evt)
ds$evt <- gsub("^WILD/(.)+", "WILDFIRE", ds$evt)
ds$evt <- gsub("^WINTER WEATHER(.)+", "WINTER WEATHER", ds$evt)


length(unique(ds$evt))

min_freq <- dim(ds)[1] * 0.001
evt_freq <- tapply(ds$evt, ds$evt, length)
events <- names(evt_freq[evt_freq > min_freq])


event_table <- toupper( c("Astronomical Low Tide", "Avalanche", "Blizzard", "Coastal Flood", "Cold/Wind Chill", 
                          "Debris Flow", "Dense Fog", "Dense Smoke", "Drought", "Dust Devil", "Dust Storm", 
                          "Excessive Heat", "Extreme Cold/Wind Chill", "Flash Flood", "Flood", "Frost/Freeze", 
                          "Funnel Cloud", "Freezing Fog", "Hail", "Heat", "Heavy Rain", "Heavy Snow", "High Surf", 
                          "High Wind", "Designator", "Event Name", "Hurricane (Typhoon)", "Ice Storm", "Lake-Effect Snow", 
                          "Lakeshore Flood", "Lightning", "Marine Hail", "Marine High Wind", "Marine Strong Wind", 
                          "Marine Thunderstorm Wind", "Rip Current", "Seiche", "Sleet", "Storm Surge/Tide", "Strong Wind", 
                          "Thunderstorm Wind", "Tornado", "Tropical Depression", "Tropical Storm", "Tsunami", "Volcanic Ash", 
                          "Waterspout", "Wildfire", "Winter Storm", "Winter Weather"))

table(ds$evt %in% event_table)

ds1 <- ds[ds$evt %in% event_table, c("evt", "FATALITIES", "INJURIES", "PROPDMG", 
                                    "PROPDMGEXP", "CROPDMG", "CROPDMGEXP")]


###########
## Injuries and fatalities
###########

## Select evtypes caused injuries and fatalities
ds1 <- ds %>% select(evt, INJURIES, FATALITIES) %>% filter(INJURIES > 0 | FATALITIES > 0)

## Diferent event types
nrow(distinct(select(ds1, evt)))

## Total and proportion of freq, injuries and fatalities by evt
ds2 <-  ds1 %>% group_by(evt) %>% 
    summarise(freq = n(), total.inj = sum(INJURIES), total.fat = sum(FATALITIES)) %>%
    mutate(prop.inj = round(total.inj / sum(total.inj), 2), prop.fat = round(total.fat / sum(total.fat), 2)) %>%
    arrange(desc(prop.fat), desc(prop.inj))

head(ds2, 20)

## Rename relevant evtypes on Storm data event table (pg 6)


ds3 <-  ds1 %>% group_by(EVTYPE) %>% 
    summarise(freq = n(), total.inj = sum(INJURIES), total.fat = sum(FATALITIES)) %>%
    mutate(prop.inj = round(total.inj / sum(total.inj), 2), prop.fat = round(total.fat / sum(total.fat), 2)) %>%
    arrange(desc(prop.fat), desc(prop.inj))

events <- ds3$EVTYPE[1:10]

ds4 <- filter(ds1, EVTYPE %in% events)

qplot(log(INJURIES), log(FATALITIES), data = ds1, colour = evt)
qplot(evt, INJURIES + FATALITIES, data = ds1, geom = "jitter") +
    theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))



############################
# Economic consequences
############################

ds10 <- ds[ ,c("evt", "PROPDMG", "PROPDMGEXP", "CROPDMG", "CROPDMGEXP")]

## Transform magnitudes to digits (pg 12)
table(ds10$PROPDMGEXP, useNA = "ifany")
ds10$pdmg[ds10$PROPDMGEXP %in% c("k", "K")] <- 1000
ds10$pdmg[ds10$PROPDMGEXP %in% c("m", "M")] <- 1e6
ds10$pdmg[ds10$PROPDMGEXP %in% c("b", "B")] <- 1e9
ds10$pdmg[is.na(ds10$pdmg)] <- 0
table(ds10$pdmg, useNA = "ifany")

table(ds10$CROPDMGEXP, useNA = "ifany")
ds10$cdmg[ds10$CROPDMGEXP %in% c("k", "K")] <- 1000
ds10$cdmg[ds10$CROPDMGEXP %in% c("m", "M")] <- 1e6
ds10$cdmg[ds10$CROPDMGEXP %in% c("b", "B")] <- 1e9
ds10$cdmg[is.na(ds10$cdmg)] <- 0
table(ds10$cdmg, useNA = "ifany")

ds10$pd <- ds10$PROPDMG * ds10$pdmg
summary(ds10$pd)

ds10$cd <- ds10$CROPDMG * ds10$cdmg
summary(ds10$cd)

ds10$td <- ds10$pd + ds10$cd
summary(ds10$td)

ds11 <- ds10[ds10$td > 0, c("evt", "pd", "cd", "td")]
head(ds11)
qplot(log(pd), log(cd), data = ds11, colour = evt)
qplot(evt, td, data = ds11, log = "y", geom = "jitter") + 
    theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
