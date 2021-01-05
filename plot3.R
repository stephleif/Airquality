##plot 3 

##Of the four types of sources indicated by the 
##type (point, nonpoint, onroad, nonroad) variable, 
##which of these four sources have seen decreases 
##in emissions from 1999–2008 for Baltimore City? 
##Which have seen increases in emissions from 1999–2008? 
##Use the ggplot2 plotting system to make a plot answer 
##this question.

plot3 <- function (datapath = "C:/Users/steph/data/"){
    
    library(dplyr)
    library(tidyverse)
    library(data.table)
    library(ggplot2)
    library(viridis)
    
    
    
    ## read in the data
    if (file.exists("summarySCC_PM25.rds") &
        file.exists("Source_Classification_Code.rds")){
        NEI <- readRDS("summarySCC_PM25.rds")
        ##SCC <- readRDS("Source_Classification_Code.rds")
    } ## files are in working directory and already unzipped
    else {
        if (file.exists ("exdata_data_NEI_data.zip") ){
            ## data is in the local working directory, unzip and load it
            unzip("exdata_data_NEI_data.zip")
            NEI <- readRDS("summarySCC_PM25.rds")
            ##SCC <- readRDS("Source_Classification_Code.rds")
        }
        else{
            ## set the working directory for data on my machine
            setwd(datapath)
            if (file.exists("summarySCC_PM25.rds") &
                file.exists("Source_Classification_Code.rds")){
                NEI <- readRDS("summarySCC_PM25.rds")
                ##SCC <- readRDS("Source_Classification_Code.rds")
            } ## files are in working directory and already unzipped
            else {
                if (file.exists ("exdata_data_NEI_data.zip") ){
                    ## data is in the local working directory, unzip and load it
                    unzip("exdata_data_NEI_data.zip")
                    NEI <- readRDS("summarySCC_PM25.rds")
                    ##SCC <- readRDS("Source_Classification_Code.rds")
                }
                else{ stop("data file not found")}
            }}}
    ## end loading in the data


    myNEI <- summarise(group_by(filter(NEI,fips == "24510"),year,type), Emissions=sum(Emissions))
    
        ggplot(myNEI, aes(x=factor(year), y=Emissions, fill=type,
                           label = round(Emissions))) +
        geom_bar(stat="identity") +
        facet_grid(. ~ type) +
            scale_color_gradientn(colours = rainbow(4))+
        labs(y="Pollution in tons", 
             title="Balimore, MD") +
        xlab("Year") +
        geom_label(aes(fill = type), colour = "black", fontface = "bold", size =3)

    ggsave(file="plot3.png", width = 8, height = 6, unit = "in")
    
}
