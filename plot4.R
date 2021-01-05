##plot4
##Across the United States, how have emissions 
##from coal combustion-related sources changed from 1999–2008?

plot4 <- function (datapath = "C:/Users/steph/data/"){
    library(dplyr)
    library(tidyverse)
    library(data.table)
    library(ggplot2)
    library(viridis)
 
    ## read in the data
    if (file.exists("summarySCC_PM25.rds") &
        file.exists("Source_Classification_Code.rds")){
        NEI <- readRDS("summarySCC_PM25.rds")
        SCC <- readRDS("Source_Classification_Code.rds")
    } ## files are in working directory and already unzipped
    else {
        if (file.exists ("exdata_data_NEI_data.zip") ){
            ## data is in the local working directory, unzip and load it
            unzip("exdata_data_NEI_data.zip")
            NEI <- readRDS("summarySCC_PM25.rds")
            SCC <- readRDS("Source_Classification_Code.rds")
        }
        else{
            ## set the working directory for data on my machine
            setwd(datapath)
            if (file.exists("summarySCC_PM25.rds") &
                file.exists("Source_Classification_Code.rds")){
                NEI <- readRDS("summarySCC_PM25.rds")
                SCC <- readRDS("Source_Classification_Code.rds")
            } ## files are in working directory and already unzipped
            else {
                if (file.exists ("exdata_data_NEI_data.zip") ){
                    ## data is in the local working directory, unzip and load it
                    unzip("exdata_data_NEI_data.zip")
                    NEI <- readRDS("summarySCC_PM25.rds")
                    SCC <- readRDS("Source_Classification_Code.rds")
                }
                else{ stop("data file not found")}
            }}}
    ## end loading in the data
    ## find the coal
    ## fix the colors

    
    coal <- grepl("Fuel Comb.*Coal", SCC$EI.Sector)
    coal.sources <- SCC[coal,]
    
    ## get just emissions from coal combustion
    ecoal.combustion <- NEI[(NEI$SCC %in% coal.sources$SCC), ]
    ecoal.related <- summarise(group_by(ecoal.combustion, year), Emissions=sum(Emissions))
    ggplot(ecoal.related, aes(x=factor(year), y=Emissions/1000, fill=year, label = round(Emissions/1000,2))) +
        geom_bar(stat="identity") +
        scale_fill_viridis()  +
        labs(y="Total emissions in kilotons", x = "Year",
             title="Emissions from coal combustion sources in kilotons")+
        geom_label(aes(fill = year),colour = "white")

    ggsave(file="plot4.png", width = 6, height = 6, unit = "in")
}## end plot 4