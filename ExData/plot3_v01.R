## v01 12-3-2015

plot3 <- function(){
  library(reshape2)
  library(ggplot2)
  
  
  ## Read file saved in working directory
  NEI <- readRDS("./summarySCC_PM25.rds")
  
  ## Calculate vars
  NEI_Baltimore <- NEI[NEI$fips == "24510", c("year", "type", "Emissions")]
  NEI_Baltimore_sum <- dcast(NEI_Baltimore, year ~ type, sum, value.var = "Emissions")
  NEI_Baltimore_melted <- melt(NEI_Baltimore_sum, id = "year",
                               measure.vars = unique(NEI_Baltimore$type),
                               variable.name = "type",
                               value.name = "Emissions")
  
  ## Plotting
  png(file = "./plot3.png")
  g <- qplot(year, Emissions, data = NEI_Baltimore_melted, color = type,
        geom = c("point", "line"))
  print(g)
  dev.off()
}