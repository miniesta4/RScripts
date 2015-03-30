library(data.table)

deaths <- readRDS("./files/deaths.rds")
dt <- data.table(deaths)
dt[!is.na(hod), .N,.(hod, cod)]

## Count deaths by cod, hod and total
hod2[, total_cod := sum(N), cod]
hod2[, total_hod := sum(N), hod]
hod2[, total := sum(N)]

## Calculate proportions
hod2 [, c("cod.hod", "hod.total") := list(N/total_cod, total_hod/total)]
