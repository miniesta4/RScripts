## v03 16-3-2015

plot6 <- function(){
  
  library(dplyr)
  library(ggplot2)

  ## Read file from working directory
  NEI <- readRDS("./summarySCC_PM25.rds")
  SCC_ds <- readRDS("Source_Classification_Code.rds")
    
  ## Select motor vehicle sources
  SCC_coal <- SCC_ds %>% filter(grepl("vehicle", tolower(SCC_ds$SCC.Level.Two))) %>%
      select(SCC)
  
  ## Calculate vars
  df <- NEI %>% filter(fips %in% c("24510", "06037"), SCC %in% SCC_coal$SCC) %>% 
    group_by(year, fips) %>% 
    summarize(PM2.5_emissions = sum(Emissions)) %>%
    mutate(fips_label = factor(fips, labels = c("Los Angeles County", "Baltimore City")))
 
  ## Plotting
  g <- qplot(year, PM2.5_emissions, data = df, facets = . ~ fips_label,
             geom = c("point", "line"))
  png(file = "./plot6_v03.png")
  print(g)
  dev.off()
}
