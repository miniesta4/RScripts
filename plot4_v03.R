## v03 13-3-2015

plot4 <- function(){
  
  library(dplyr)
  library(ggplot2)

  ## Read file from working directory
  NEI <- readRDS("./summarySCC_PM25.rds")
  SCC_ds <- readRDS("Source_Classification_Code.rds")
    
  ## Select coal combustion sources
  SCC_coal <- SCC_ds %>% filter(grepl("Comb(.)*Coal",SCC_ds$Short.Name)) %>% select(SCC)
  
  ## Calculate vars
  df <- NEI %>% filter(SCC %in% SCC_coal$SCC) %>% group_by(year) %>% 
    summarize(PM2.5_emissions = sum(Emissions))
 
  ## Plotting
  g <- qplot(year, PM2.5_emissions, data = df, geom = c("point", "line"))
  png(file = "./plot4_v03.png")
  print(g)
  dev.off()
}
