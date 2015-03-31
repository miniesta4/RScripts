tidyData <- function(){
## Load library
library(data.table)

## Read files
deaths <- readRDS("../files/deaths.rds")
dt <- data.table(deaths)
dt <- dt[!is.na(hod), .(count = .N), .(hod, cod)]

ics <- fread("../files/icd-main.csv")
ics <- unique(ics)

## Join dt, ics
setkey(dt, cod)
setkey(ics, code)
dt_ics <- dt[, .SD[ics[code %in% .SD$cod]], hod]

## Proportions by cod
dt_ics[, prop.cod := count/sum(count), cod]

## Proportions total
dt_total <- dt[!is.na(hod), .(count = .N), .(hod)]
dt_total[, prop.total := count/sum(count)]

## Write to csv
## write.csv2(dt_ics, file = "./td_out.csv", row.names = FALSE)

## Plotting
t <- as.character(Sys.time())
t <- gsub("[ :-]", "", t)
n <- paste0("./td_", t, "_v02.png")
png(n)
with(dt_total, plot(hod, prop.total, ylim = c(0, 0.15), xlab = "Hour", ylab = "Proportion"))
with(dt_ics[cod == ("A01")], points(hod, prop.cod, col = "red"))
with(dt_ics[cod == ("A02")], points(hod, prop.cod, col = "blue"))
title(main = "Deaths by hour")
legend("topright", pch = 1, col = c("black", "red"), legend = c("Total", "A01", "A02"))
dev.off()
}
