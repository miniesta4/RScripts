## v01 12-3-2015

plot2 <- function(){
  ## Read file saved in working directory
  NEI <- readRDS("./summarySCC_PM25.rds")
  
  ## Calculate vars
  NEI_Baltimore <- NEI[NEI$fips == "24510", c("year", "Emissions")]
  emissions <- with(NEI_Baltimore, tapply(Emissions, year, sum))
  years <- as.numeric(names(emissions))
  df <- data.frame(years, emissions)
  
  ## Plotting
  png(file = "./plot2.png")
  with (df, plot(years, emissions, type = "b",
                 xlab = "Year", ylab = "PM2.5 (in tons)", 
                 main = "Total PM2.5 emissions in Baltimore City"))
  dev.off()
}