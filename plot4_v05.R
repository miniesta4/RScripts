## v05 19-3-2015

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
    summarize(PM2.5_emissions = sum(Emissions))
 
  ## Plotting
  g <- qplot(year, PM2.5_emissions, data = df, geom = c("point", "line"))
  png(file = "./plot4_v05.png")
  print(g)
  dev.off()
}
