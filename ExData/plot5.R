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
    summarize(emissions = sum(Emissions))
 
  ## Plotting
  g <- qplot(year, emissions, data = df, geom = c("point", "line"),
             main = "Emissions from motor vehicle sources in Baltimore City",
             ylab = "PM2.5 (in tons)")
  png(file = "./plot5.png")
  print(g)
  dev.off()
}
