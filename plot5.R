## plot 5
##How have emissions from motor vehicle sources changed 
## from 1999–2008 in Baltimore City?
plot5 <- function (datapath = "C:/Users/steph/data/"){
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
    ## find the coal
    ## fix the colors
    
    
    motor <- !grepl("*Non-Road|*Bulk Gasoline Terminals|*Stations|*Terminals|*Production|*Gas Stations",SCC$EI.Sector) & 
        grepl("*Motor|*motor|*vehicle|*Vehicle|*car|*Car|*On-Road|*Diesel|*diesel", SCC$EI.Sector)
    
    motor.sources <- SCC[motor,]
    ##print(dim(motor.sources))
    
    ## get just emissions from coal combustion
    motorpollution <- NEI[(NEI$SCC %in% motor.sources$SCC), ]
    motor.related <- summarise(group_by(filter(motorpollution, fips == "24510"),
                                         year), Emissions=sum(Emissions))
    ggplot(motor.related, aes(x=factor(year), y=Emissions/1000, 
                              fill=year, label = round(Emissions/1000,2))) +
        geom_bar(stat="identity") + 
        labs(y="Total emissions in kilotons", x = "Year",
             title="Motor related emissions in kilotons")+
        geom_label(aes(fill = year),colour = "white", size =3)
    
    ggsave(file="plot5.png", width = 6, height = 6, unit = "in")
}## end plot 5