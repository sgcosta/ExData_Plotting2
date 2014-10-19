#### Project: Project 2 Coursera 
#### Course: "Exploratory Data Analysis"
#### Date: October 18, 2014
#### Author: Costa, S.
#### Plot: Plot 4
#### Across the United States, how have emissions from coal combustion-related sources changed from 1999–2008?

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

# Read and Clean Data (a hook function for handling reference objects)
SCC <- readRDS(SCC)
NEI <- readRDS(NEI)

# Coal combustion-related sources
# Find emissions from coal combustion-related sources
CombustionCoal <- NEI[NEI$SCC %in% SCC[grep("Coal", SCC$EI.Sector), 1], ]
CombustionCoalSources <- SCC[, c(1, 4)]
Coal <- merge(CombustionCoal, CombustionCoalSources, by.x = "SCC", by.y = "SCC")[, c(4, 6, 7)]

#Aggregate Emmisions by Year and EI.Sector
Coal <- aggregate(Emissions ~ year + EI.Sector, Coal, sum)

# Plot 4
## Construct the plot and save it to a PNG file with a width of 780 pixels and a height of 580 pixels (units).
png("plot4.png", 
    width  = 780, 
    height = 580, 
    units  = 'px', 
    bg     = 'white')

# Construct the plot using the ggplot function
ggplot(Coal, aes(x = factor(year), y = Emissions/100000, fill = Coal$EI.Sector)) +
    geom_bar(stat = "identity", ) +
    guides(fill = guide_legend(title = "Coal Combustion-Related Sources")) +
    scale_fill_brewer() +
    xlab("Year") +
    ylab(expression("Total PM"[2.5] * " Emission [in Million Tons]")) +
    ggtitle(expression("Total PM"[2.5] * " Emissions from Coal Combustion-Related Sources (1999 - 2008)"))
            
dev.off()
