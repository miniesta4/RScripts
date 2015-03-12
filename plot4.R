## v01 12-3-2015

plot4 <- function(){
  ## Read file saved in working directory
  NEI <- readRDS("./summarySCC_PM25.rds")
  SCC <- readRDS("Source_Classification_Code.rds")
    
  SCC_coal_index <- SCC[grep("Comb(.)*Coal",SCC$Short.Name),"SCC"]
  SCC_coal <- SCC[SCC_coal_index, "SCC"]
  NEI_coal <- NEI[NEI$SCC %in% SCC_coal, c("year", "fips", "Emissions")]
  
  
  ## Plotting
  png(file = "./plot4.png")
  with (NEI_coal, plot(year, Emissions, type = "p", 
                 xlab = "Year", ylab = "PM2.5 (in tons)",
                 main = "Total PM2.5 emissions from coal combustion-related 
                 sources accross the US"))
  dev.off()
}