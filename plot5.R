node <- "/Users/anna/OneDrive/Coursera/JHU - Data Science/"
localPath <- "4. Exploratory Data Analysis/Week 03/Coursera_JHU_4EDA_project_2/"
setwd(paste(node, localPath, sep = ""))
source("Init.R")

# Q5
# How have emissions from motor vehicle sources changed from 1999–2008 in Baltimore City?

# Answer
# I approached this question in the same way as a question 4.  It is interesting
# to see that trends for Total Emission and Mean Emission are quite similar. But
# trend for median emission is very different. I am not sure what to think of
# it. I do know that I have only 88 records in Baltimore to summarize, maybe it
# is not large enough sample to have similar trend for all three variables.


# Check various fields that might have words Motor and Veh in them. 
# Get a feel on how to get all data related to Motor Vehicles
str(SCC)
sapply(colnames(SCC), getSCC_Rows, "Motor", "Veh", FALSE)
# By doing this I just find out that the following columns have words "Motor" and
# "Veh" in their values: $Short.Name and $SCC.Level.Three

# Get only those records from SCC that have words "Motor" and "Veh" 
# in at least one of those 2 fields
tmp1 <- getSCC_Rows("Short.Name", "Motor", "Veh")
tmp2 <- getSCC_Rows("SCC.Level.Three", "Motor", "Veh")
subsetSCC <- unique(rbind(tmp1, tmp2))
colnames(subsetSCC) <- colnames(SCC)

# Now I will merge NEI_Baltimore data and subsetSCC data
mergedData <- merge(subsetSCC, NEI_Baltimore)

# start graphing
png(filename = "plot5.png", width = 600, height = 1800)
par(mfrow = c(3, 1))

# as in previous plots data are heavily skewed towards 0 value, so regular plot/
# boxplot poduce graphs that are very hard to interpret. Instead of doing regular
# graphs I want to show change in total, mean and median by year
mergedData <- group_by(mergedData, year)
mergeByYear <- summarize(mergedData, sumByYear = sum(Emissions, na.rm = TRUE),
                         meanByYear = mean(Emissions, na.rm = TRUE),
                         medianByYear = median(Emissions, na.rm = TRUE))

plot(mergeByYear$year, mergeByYear$sumByYear, col = "black", type = "b", xlab = "", 
     main = "Baltimore Total Emissions by Year from Motor Vehicle  Sources",
     ylab = " Values")
plot(mergeByYear$year, mergeByYear$medianByYear, col = "black", type = "b", xlab = "", 
     main = "Baltimore Median Emissions by Year from Motor Vehicle  Sources",
     ylab = " Values")
plot(mergeByYear$year, mergeByYear$meanByYear, col = "black", type = "b", xlab = "", 
     main = "Baltimore Mean Emissions by Year from Motor Vehicle  Sources",
     ylab = " Values")

dev.off()