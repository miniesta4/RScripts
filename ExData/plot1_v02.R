## v02 13-3-2015

plot1 <- function(){
  
  library(dplyr)

  ## Read file from working directory
  if (!exists("NEI")) NEI <- readRDS("./summarySCC_PM25.rds")
  
  ## Calculate vars
  df <- NEI %>% group_by(year) %>% summarize(emissions = sum(Emissions))
  
  ## Plotting
  png(file = "./plot1.png")
  with (df, plot(year, emissions, type = "b", 
                 xlab = "Year", ylab = "PM2.5 (in tons)",
                 main = "Total PM2.5 emissions in the US"))
  dev.off()
}