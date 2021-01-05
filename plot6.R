## plot 6
##Compare emissions from motor vehicle sources in Baltimore City with 
##emissions from motor vehicle sources in Los Angeles County, California
##(\color{red}{\verb|fips == "06037"|}fips == ""). Which city has seen 
##greater changes over time in motor vehicle emissions?



plot6 <- function (datapath = "C:/Users/steph/data/"){
    library(dplyr)
    library(tidyverse)
    library(data.table)
    library(ggplot2)
    
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

    
    
    motor <- !grepl("*Non-Road|*Bulk Gasoline Terminals|*Stations|*Terminals|*Production|*Gas Stations",SCC$EI.Sector) & 
        grepl("*Motor|*motor|*vehicle|*Vehicle|*car|*Car|*On-Road|*Diesel|*diesel", SCC$EI.Sector)
    
    motor.sources <- SCC[motor,]
    PlaceFactor <- factor(c("Baltimore","Los Angeles"))
    motorpollution <- NEI[(NEI$SCC %in% motor.sources$SCC), ]
    motorpollution <- motorpollution %>% filter(fips=="24510"| fips=="06037" )
    motorpollution <- motorpollution %>% 
        mutate(place = ifelse(fips =="24510", "Baltimore", "Los Angeles"))
    motorpollution <- summarise(group_by(filter(motorpollution, !is.na(place) ),place,year), 
                Emissions=sum(Emissions))
 
    ggplot(motorpollution, aes(x=factor(year), y=Emissions/1000, 
                              fill= year, label = round(Emissions/1000,2))) +
        geom_bar(stat="identity") + 
        facet_grid(place~.) +
        labs(y="Total emissions in kilotons", x = "Year",
             title="Motor Emissions comparison between Los Angeles and Baltimore",
             subtitle = "Baltimore has lower emissions than Los Angeles")+
        theme(legend.position = "none") +
        geom_label(aes(fill = year),colour = "white", size =3) 


    
    ggsave(file="plot6.png", width = 7, height = 7, unit = "in")
}## end plot 6