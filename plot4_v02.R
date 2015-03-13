## v02 13-3-2015

plot4 <- function(){
  
  library(dplyr)

  ## Read file from working directory
  NEI <- readRDS("./summarySCC_PM25.rds")
  SCC <- readRDS("Source_Classification_Code.rds")
    
  ## Select coal combustion sources
  SCC_coal <- SCC %>% filter(grep("Comb(.)*Coal",SCC$Short.Name)) %>% select(SCC)
  
  ## Calculate vars
  df <- NEI %>% filter(SCC %in% SCC_coal) %>% group_by(year) %>% summarize(emissions = Emissions)
 
  ## Plotting
  png(file = "./plot4_v02.png")
  with (df, plot(years, df, type = "b", 
                 xlab = "Year", ylab = "PM2.5 (in tons)",
                 main = "Total PM2.5 emissions from coal combustion-related 
                 sources accross the US"))
  dev.off()
}
