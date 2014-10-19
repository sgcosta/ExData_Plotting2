#### Project: Project 2 Coursera 
#### Course: "Exploratory Data Analysis"
#### Date: October 19, 2014
#### Author: Costa, S.
#### Plot: Plot 5
#### How have emissions from motor vehicle sources changed from 1999–2008 in Baltimore City?

# Use ggplot2 library
library(ggplot2)

## Set VariableS
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

# Get Baltimore emissions from motor vehicle sources
BaltimoreVehicles <- NEIData[NEIData$fips == "24510" & (NEIData$type == "ON-ROAD"), ]

# Get Combustion Sources :: emissions from motor vehicle sources
CombustionSources <- SCCData[, c(1, 4)]

# Baltimore emissions from motor vehicle sources Merge Join by SCC Type
BaltimoreVehicles <- merge(BaltimoreVehicles, CombustionSources, by.x = "SCC", by.y = "SCC")[, c(4, 6, 7)]

## Aggregate Emmisions by Year and EI Sector
BaltimoreVehicles <- aggregate(Emissions ~ year + EI.Sector, BaltimoreVehicles, sum)

# Plot 5
## Construct the plot and save it to a PNG file with a width of 780 pixels and a height of 580 pixels (units).
png("plot5.png", 
    width  = 780, 
    height = 580, 
    units  = 'px', 
    bg     = 'white')

# Construct the plot using the ggplot function
ggplot(BaltimoreVehicles, aes(x = factor(year), y = Emissions, fill = BaltimoreVehicles$EI.Sector)) +
    geom_bar(stat = "identity", ) +
    guides(fill = guide_legend(title = "Combustion-Related")) +
    scale_fill_brewer() +
    xlab("Year") +
    ylab(expression("Total PM"[2.5] * " Emission [in Million Tons]")) +
    ggtitle(expression("Total PM"[2.5] * " Emissions from Motor Vehicle Sources in Baltimore City (1999 - 2008)"))

dev.off()