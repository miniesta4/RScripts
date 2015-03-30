## v01 15-3-2015

plot5 <- function(){
  
  library(dplyr)
  library(ggplot2)

  ## Read file from working directory
  if (!exists("NEI")) NEI <- readRDS("./summarySCC_PM25.rds")
  if (!exists("SCC_ds")) SCC_ds <- readRDS("Source_Classification_Code.rds")
    
  ## Select motor vehicle sources
  SCC_coal <- SCC_ds %>% filter(grepl("vehicle", tolower(SCC_ds$SCC.Level.Two))) %>%
      select(SCC)
  
  ## Calculate vars
  df <- NEI %>% filter(fips == "24510", SCC %in% SCC_coal$SCC) %>% group_by(year) %>% 
    summarize(PM2.5_emissions = sum(Emissions))
 
  ## Plotting
  g <- qplot(year, PM2.5_emissions, data = df, geom = c("point", "line"))
  png(file = "./plot5_v01.png")
  print(g)
  dev.off()
}
