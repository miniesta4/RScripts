plot2 <- function(){
  
  library(dplyr)
  
  ## Read file from working directory
  if (!exists("NEI")) NEI <- readRDS("./summarySCC_PM25.rds")
  
  ## Calculate vars  
  df <- NEI %>% filter(fips == "24510") %>% group_by(year) %>%  
    summarize(emissions = sum(Emissions))
  
  ## Plotting
  png(file = "./plot2.png")
  with (df, plot(year, emissions, type = "b",
                 xlab = "Year", ylab = "PM2.5 (in tons)", 
                 main = "Total PM2.5 emissions in Baltimore City"))
  dev.off()
}