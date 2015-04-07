plot3 <- function(){

  library(dplyr)
  library(ggplot2)
  
  ## Read file saved in working directory
  if (!exists("NEI")) NEI <- readRDS("./summarySCC_PM25.rds")
  
  ## Calculate vars
  df <- NEI %>% filter(fips == "24510") %>% group_by(year, type) %>%
    summarize(emissions = sum(Emissions))
  
  ## Plotting
  g <- qplot(year, emissions, data = df, color = type,
        geom = c("point", "line"), main = "PM2.5 emissions in Baltimore City by type",
        ylab = "PM2.5 (in tons)")
  png(file = "./plot3.png")
  print(g)
  dev.off()
}