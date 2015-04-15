tidyData <- function(){

  library(data.table)
  
  ## Read deaths
  deaths <- readRDS("./files/deaths.rds")
  dt <- data.table(deaths)
  rm(deaths)
  dt <- dt[!is.na(hod), .N, .(hod, cod)]

  ## Read death causes
  icd <- fread("./files/icd-main.csv")
  
  ## Set keys and join
  setkey(dt, cod)
  setkey(icd, code)
  hod2 <- dt[, .SD[icd[code %in% .SD$cod]], hod]
  
  ## Count deaths by cod, hod and total
  hod2[, total_cod := sum(N), cod]
  hod2[, total_hod := sum(N), hod]
  hod2[, total := sum(N)]
  
  ## Calculate proportions
  hod2 [, c("cod.hod", "hod.total") := list(N/total_cod, total_hod/total)]
  
}

