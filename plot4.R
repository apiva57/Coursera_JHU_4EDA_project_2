node <- "/Users/anna/OneDrive/Coursera/JHU - Data Science/"
localPath <- "4. Exploratory Data Analysis/Week 03/Project 2/"
setwd(paste(node, localPath, sep = ""))
source("Init.R")

# Q4 
# Across the United States, how have emissions from coal combustion-related
# sources changed from 1999–2008?

# Check various fields that might have words coal and comb in them. 
# Get a feel on how to get all data related to coal and combustion
str(SCC)
sapply(colnames(SCC), getSCC_Rows, "Coal", "Comb", FALSE)
# By doing this I just find out that the following columns have words 
# "Coal" and "Comb" in their values: $Short.Name, $EI.Sector, 
# and $SCC.Level.Four

# Get only those records from SCC that have words "Coal" and "Comb" 
# in at least one of those 3 fields
tmp1 <- getSCC_Rows("Short.Name", "Coal", "Comb")
tmp2 <- getSCC_Rows("EI.Sector", "Coal", "Comb")
tmp3 <- getSCC_Rows("SCC.Level.Four", "Coal", "Comb")
subsetSCC <- unique(rbind(tmp1, tmp2, tmp3))
colnames(subsetSCC) <- colnames(SCC)

# Now I will merger NEI data and subsetSCC data
mergedData <- merge(subsetSCC, NEI)

# start graphing
png(filename = "plot4.png", width = 600, height = 1800)
par(mfrow = c(3, 1))

# as in previous plots data are heavily skewed towards 0 value, so regular plot/
# boxplot poduce graphs that are very hard to interpret. Instead of doing regular
# graphs I want to show change in total, mean and median by year
mergedData <- group_by(mergedData, year)
mergeByYear <- summarize(mergedData, sumByYear = sum(Emissions, na.rm = TRUE),
                         meanByYear = mean(Emissions, na.rm = TRUE),
                         medianByYear = median(Emissions, na.rm = TRUE))

plot(mergeByYear$year, mergeByYear$sumByYear, col = "black", type = "b", xlab = "", 
     main = "USA Total Emissions by Year from Coal Combustion Sources",
     ylab = " Values")
plot(mergeByYear$year, mergeByYear$medianByYear, col = "black", type = "b", xlab = "", 
     main = "USA Median Emissions by Year from Coal Combustion Sources",
     ylab = " Values")
plot(mergeByYear$year, mergeByYear$meanByYear, col = "black", type = "b", xlab = "", 
     main = "USA Mean Emissions by Year from Coal Combustion Sources",
     ylab = " Values")

dev.off()