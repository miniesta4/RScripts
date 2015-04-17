plot6 <- function(){
    
    library(dplyr)
    library(ggplot2)
    
    ## Read file from working directory
    if (!exists("NEI")) NEI <- readRDS("./summarySCC_PM25.rds")
    if (!exists("SCC_ds")) SCC_ds <- readRDS("Source_Classification_Code.rds")
    
    ## Select motor vehicle sources
    SCC_coal <- SCC_ds %>% filter(grepl("vehicle", tolower(SCC_ds$SCC.Level.Two))) %>%
        select(SCC)
    
    ## Calculate vars
    df <- NEI %>% filter(fips %in% c("24510", "06037"), SCC %in% SCC_coal$SCC) %>% 
        group_by(year, fips) %>% 
        summarize(emissions = sum(Emissions)) %>%
        mutate(US_county = factor(fips, labels = c("Los Angeles County", "Baltimore City")))
    
    df_p <- df %>% group_by(fips) %>% mutate(emissions.pcj = (emissions - emissions[1]) / emissions[1])
    
    ## Plotting
    g <- qplot(year, emissions.pcj, data = df_p, color = US_county,
               geom = c("point", "line"),
               main = "Emissions from motor vehicle sources",
               ylab = "PM2.5 (variation %)")
    png(file = "./plot6_v02.png")
    print(g)
    dev.off()
}
