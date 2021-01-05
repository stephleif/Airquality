plot1 <- function (datapath = "C:/Users/steph/data/"){
    
##Have total emissions from PM2.5 decreased in the United States 
##from 1999 to 2008? Using the base plotting system, make a plot 
##showing the total PM2.5 emission from all sources for each of 
##the years 1999, 2002, 2005, and 2008.

        library(dplyr)
        library(tidyverse)
        library(data.table)

        ## Note plot 1 doesn't use source classification

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
    ##print("data is loaded")        
    ##total PM2.5 emission from all sources for each of 
    ##the years 1999, 2002, 2005, and 2008 
    
    years <- c("1999","2002","2005","2008")
    ## print(tail(NEI$Pollutant))
    
    myNEI <- NEI %>% filter((Pollutant == "PM25-PRI") & (year %in% years)) %>%
                  transmute(year=year,Emissions = Emissions) %>%
                  group_by(year) %>%
                  summarize(Total.Emissions = sum(Emissions, na.rm = TRUE))
        
    ##print("data is filtered")
    # plot data 
    png(filename = "plot1.png")
    with(myNEI, 
         plot(x = year, 
              y = Total.Emissions, 
              ylab = "Total Annual Emissions [Tons]", 
              xlab = "Year",
              main = "Total Annual Emissions in the US by Year",
              cex = 4,
              pch = 16,
              col = "green",
              lwd = 4))
    dev.off()  ## close png file

}## end plot1