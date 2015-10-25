node <- "/Users/anna/OneDrive/Coursera/JHU - Data Science/"
localPath <- "4. Exploratory Data Analysis/Week 03/Coursera_JHU_4EDA_project_2/"
setwd(paste(node, localPath, sep = ""))
source("Init.R")

# Q6
# Compare emissions from motor vehicle sources in Baltimore City with emissions
# from motor vehicle sources in Los Angeles County, California (fips ==
# "06037"). Which city has seen greater changes over time in motor vehicle
# emissions?

# I will be reusing subsetSCC that I constructed in plot5.R

# Now I will merger NEI_BandLA data and subsetSCC data
mergedData <- merge(subsetSCC, NEI_BandLA)

# start graphing
png(filename = "plot6.png", width = 600, height = 1800)


dev.off()