## v01 12-3-2015

plot1 <- function(){
  ## Read file saved in working directory
  NEI <- readRDS("./summarySCC_PM25.rds")
  
  ## Calculate vars
  emissions <- tapply(NEI$Emissions, NEI$year, sum)
  years <- as.numeric(names(emissions))
  df <- data.frame(years, emissions)
  
  ## Plotting
  png(file = "./plot1.png")
  with (df, plot(years, emissions, type = "b", 
                 xlab = "Year", ylab = "PM2.5 (in tons)",
                 main = "Total PM2.5 emissions in the US"))
  dev.off()
}