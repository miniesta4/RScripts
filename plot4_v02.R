## v02 13-3-2015

plot4 <- function(){
  
  library(dplyr)

  ## Read file from working directory
  NEI <- readRDS("./summarySCC_PM25.rds")
  SCC_ds <- readRDS("Source_Classification_Code.rds")
    
  ## Select coal combustion sources
  SCC_coal <- SCC_ds %>% filter(grepl("Comb(.)*Coal",SCC_ds$Short.Name)) %>% select(SCC)
  
  ## Calculate vars
  df <- NEI %>% filter(SCC %in% SCC_coal$SCC) %>% group_by(year) %>% 
    summarize(emissions = sum(Emissions))
 
  ## Plotting
  png(file = "./plot4_v02.png")
  with (df, plot(year, emissions, type = "b", 
             xlab = "Year", ylab = "PM2.5 (in tons)",
             main = "Total PM2.5 emissions from coal combustion-related 
             sources accross the US"))
  dev.off()
}
