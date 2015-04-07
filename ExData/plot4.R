plot4 <- function(){
  
  library(dplyr)
  library(ggplot2)

  ## Read file from working directory
  if (!exists("NEI")) NEI <- readRDS("./summarySCC_PM25.rds")
  if (!exists("SCC_ds")) SCC_ds <- readRDS("Source_Classification_Code.rds")
    
  ## Select coal combustion sources
  SCC_coal <- SCC_ds %>% filter(grepl("comb(.)*coal",tolower(SCC_ds$EI.Sector))) %>% select(SCC)
  
  ## Calculate vars
  df <- NEI %>% filter(SCC %in% SCC_coal$SCC) %>% group_by(year) %>% 
    summarize(emissions = sum(Emissions))
 
  ## Plotting
  g <- qplot(year, emissions, data = df, geom = c("point", "line"),
             main = "Emissions from coal combustion-related sources in the US",
             ylab = "PM2.5 (in tons)")
  png(file = "./plot4.png")
  print(g)
  dev.off()
}
