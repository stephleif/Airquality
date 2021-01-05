##plot2
##Have total emissions from PM2.5 decreased in the 
## Baltimore City, Maryland fips == "24510" from 
##1999 to 2008? Use the base plotting system to 
##make a plot answering this question.
plot2 <- function (datapath = "C:/Users/steph/data/"){
   
    library(dplyr)
    library(tidyverse)
    library(data.table)
   
    ## read in the data
    if (file.exists("summarySCC_PM25.rds") &
        file.exists("Source_Classification_Code.rds")){
        NEI <- readRDS("summarySCC_PM25.rds")
    ##    SCC <- readRDS("Source_Classification_Code.rds")
    } ## files are in working directory and already unzipped
    else {
        if (file.exists ("exdata_data_NEI_data.zip") ){
            ## data is in the local working directory, unzip and load it
            unzip("exdata_data_NEI_data.zip")
            NEI <- readRDS("summarySCC_PM25.rds")
         ##   SCC <- readRDS("Source_Classification_Code.rds")
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
                  ##  SCC <- readRDS("Source_Classification_Code.rds")
                }
                else{ stop("data file not found")}
            }}}
    ## end loading in the data
    print("data is loaded") 
    ##Have total emissions from PM2.5 decreased in the 
    ## Baltimore City, Maryland fips == "24510"|}fips == "24510") from 
    ##1999 to 2008?  
    
    
    ## print(tail(NEI$Pollutant))
    
    myNEI <- NEI %>% filter((Pollutant == "PM25-PRI") & 
                            (year %in% 1999:2008)& 
                            (fips == "24510")) %>%
        transmute(year=year,Emissions = Emissions) %>%
        group_by(year) %>%
        summarize(Total.Emissions = sum(Emissions, na.rm = TRUE))
    
        print("data is filtered")
        ## plot data 
        png(filename = "plot2.png")
        with(myNEI, 
         plot(x = year, 
              y = Total.Emissions, 
              ylab = "Total Annual Emissions [Tons]", 
              xlab = "Year",
              main = "Total Annual Emissions in Baltimore by Year",
              cex = 4,
              pch = 16,
              col = "green",
              lwd = 2))
    dev.off()  ## close png file
    
}## end plot2