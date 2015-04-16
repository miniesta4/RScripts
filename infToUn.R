infToUn <- function(){
  ## Load library
  library(xlsx)
  
  ## Read files
  partes <- read.csv2("../files/20150415-PartesSubc.csv", colClasses = "character")
  facturas <- read.csv2("../files/20150415-FacturasSubc.csv", colClasses = "character")
  
  ## Names
  names(facturas) <- names(partes)
  
  ## Combine rows
  pyf <- rbind(partes, facturas)
  
  ## Split
  pyfs <- split(pyf, pyf$CO_ENTIDAD)
  t <- gsub("[ :-]", "", Sys.time())
  d <- paste0("../files/output/", t)
  dir.create(d)
  #lapply(pyfs, function(x) write.csv2(x, file = paste0(d, "/", x[1,2], ".csv"),
  #                                    row.names = FALSE, quote = FALSE))
  lapply(pyfs, function(x) write.xlsx2(x, file = paste0(d, "/", x[1,2], ".xlsx"), 
                                       sheetName = "Datos", row.names = FALSE, quote = FALSE))
}