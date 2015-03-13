## v02 13-3-2015

plot3 <- function(){

  library(dplyr)
  library(ggplot2)
  
  ## Read file saved in working directory
  NEI <- readRDS("./summarySCC_PM25.rds")
  
  ## Calculate vars
  df <- NEI %>% filter(fips == "24510") %>% group_by(year, type) %>% summarize(emissions = sum(Emissions))
  
  ## Plotting
  png(file = "./plot3_v02.png")
  g <- qplot(year, emissions, data = df, color = type,
        geom = c("point", "line"))
  print(g)
  dev.off()
}