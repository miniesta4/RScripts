## v01 12-3-2015

plot4 <- function(){
  ## Read file saved in working directory
  NEI <- readRDS("./summarySCC_PM25.rds")
  SCC <- readRDS("Source_Classification_Code.rds")
    
  SCC_coal <- SCC[grep("Comb(.)*Coal",SCC$Short.Name),"SCC"]
  NEI_coal <- NEI[NEI$SCC %in% SCC_coal, c("year", "Emissions")]
  NEI_coal_sum <- with(NEI_coal, tapply(Emissions, year, sum))
  years <- as.numeric(names(NEI_coal_sum))
  df <- data.frame(years, NEI_coal_sum)
  
  
  ## Plotting
  png(file = "./plot4.png")
  with (df, plot(years, NEI_coal_sum, type = "b", 
                 xlab = "Year", ylab = "PM2.5 (in tons)",
                 main = "Total PM2.5 emissions from coal combustion-related 
                 sources accross the US"))
  dev.off()
}
