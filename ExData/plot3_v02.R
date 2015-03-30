## v02 13-3-2015

plot3 <- function(){

  library(dplyr)
  library(ggplot2)
  
  ## Read file saved in working directory
  if (!exists("NEI")) NEI <- readRDS("./summarySCC_PM25.rds")
  
  ## Calculate vars
  df <- NEI %>% filter(fips == "24510") %>% group_by(year, type) %>%
    summarize(PM2.5_emissions = sum(Emissions))
  
  ## Plotting
  g <- qplot(year, PM2.5_emissions, data = df, color = type,
        geom = c("point", "line"))
  png(file = "./plot3_v02.png")
  print(g)
  dev.off()
}