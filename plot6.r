#### Project: Project 2 Coursera 
#### Course: "Exploratory Data Analysis"
#### Date: October 19, 2014
#### Author: Costa, S.
#### Plot: Plot 6
#### Compare emissions from motor vehicle sources in Baltimore City with emissions from motor vehicle sources in Los Angeles County, California (fips == "06037"). 
#### Which city has seen greater changes over time in motor vehicle emissions?

# Use ggplot2 library
library(ggplot2)

## Set Variables
# File URL to download to Data Set
fileURL <- 'https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip'

# Local data file
dataFileZIP <- "./exdata-data-NEI_data"

# Directory
dirFile <- "./Project"

# Directory and filenames (rds) of the clean data
SCC <- "./Source_Classification_Code.rds"
NEI <- "./summarySCC_PM25.rds"

# Download the dataset (.ZIP), which does not exist
if (file.exists(dataFileZIP) == FALSE) {
    download.file(fileURL, destfile = dataFileZIP)
}

# UnZIP data file
if (file.exists(dirFile) == FALSE) {
    unzip(dataFileZIP)
}

## Read and Clean Data (a hook function for handling reference objects)
# Read Data
SCCData <- readRDS(SCC, refhook = NULL)
NEIData <- readRDS(NEI, refhook = NULL)

# Get Combustion Sources emissions 
CombustionSources <- SCCData[, c(1, 4)]

# Baltimore:: Get emissions from motor vehicle sources
BaltimoreVehicles <- NEIData[NEIData$fips == "24510" & (NEIData$type == "ON-ROAD"), ]
# Baltimore:: emissions from motor vehicle sources Merge Join by SCC Type
#BaltimoreVehicles <- merge(BaltimoreVehicles, CombustionSources, by.x = "SCC", by.y = "SCC")[, c(4, 6, 7)]
# Baltimore:: Aggregate Emmisions by Year
BaltimoreVehicles <- aggregate(Emissions ~ year, BaltimoreVehicles, sum)
# Baltimore:: Add City Column
BaltimoreVehicles["City"] <- "Baltimore City"

# Los Angeles County:: Get emissions from motor vehicle sources
LosAngelesVehicles <- NEIData[NEIData$fips == "06037" & (NEIData$type == "ON-ROAD"), ]
# Los Angeles County:: Emissions from Motor Vehicle Sources Merge Join by SCC Type
#LosAngelesVehicles <- merge(LosAngelesVehicles, CombustionSources, by.x = "SCC", by.y = "SCC")[, c(4, 6, 7)]
# Los Angeles County:: Aggregate Emmisions by Year
LosAngelesVehicles <- aggregate(Emissions ~ year, LosAngelesVehicles, sum)
# Los Angeles County:: Add City Column
LosAngelesVehicles["City"] <- "Los Angeles County"

# All city
EmissionsCity <- rbind(BaltimoreVehicles, LosAngelesVehicles)

# Plot 6
## Construct the plot and save it to a PNG file with a width of 580 pixels and a height of 380 pixels (units).
png("plot6.png", 
    width  = 580, 
    height = 380, 
    units  = 'px', 
    bg     = 'white')

# Construct the plot using the ggplot function
ggplot(EmissionsCity, aes(x = factor(year), y = Emissions, fill = City)) +
    geom_bar(stat = "identity", position = position_dodge()) +
    xlab("Year") +
    ylab(expression("Total PM"[2.5] * " Emission [in Tons]")) +
    ggtitle(expression("Total PM"[2.5] * " Emissions from Motor Vehicle Sources by City (1999 - 2008)"))

dev.off()