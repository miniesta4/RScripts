tidyData <- function(){

## Load libraries
library(data.table)
library(ggplot2)

## Read files
deaths <- readRDS("../files/deaths.rds")
d <- data.table(deaths)
dt <- d[!is.na(hod), .(freq = .N), .(hod, cod)]

ics <- fread("../files/icd-main.csv")
ics <- unique(ics)

## Join dt, ics
## setkey(dt, cod)
## setkey(ics, code)
## dt_ics <- dt[, .SD[ics[code %in% .SD$cod]], hod]

## Proportions by cod
dt[, prop := freq/sum(freq), cod]

## Proportions total
dt_h <- d[!is.na(hod), .(freq_all = sum(.N)), hod]
dt_h <- dt_h[, prop_all := freq_all/sum(freq_all)]

## Join proportions: by cod, total
setkey(dt, hod)
setkey(dt_h, hod)

dt_hc <- dt[, .SD[dt_h], cod][!is.na(freq)]
dt_hc[, c("n_cod", "dist") := list(sum(freq), mean((prop - prop_all)^2)), cod]

devi <- dt_hc[n_cod > 50]

## Write to csv
## write.csv2(dt_ics, file = "./td_out.csv", row.names = FALSE)

## Plotting
## g <- qplot(x = n_cod, y = dist, data = devi, main = "Estimator vs deviation")
h <- qplot(x = n_cod, y = dist, data = devi, log = "xy", main = "Estimator vs deviation log")

## Dev
t <- as.character(Sys.time())
t <- gsub("[ :-]", "", t)
n <- paste0("./td_", t, "_v02.png")
png(n)
## print(g)
print(h)
dev.off()
}
