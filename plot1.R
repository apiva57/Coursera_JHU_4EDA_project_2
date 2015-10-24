# Q 1
# Have total emissions from PM2.5 decreased in the United States from 1999 to
# 2008? Using the base plotting system, make a plot showing the total PM2.5
# emission from all sources for each of the years 1999, 2002, 2005, and 2008. 

# Project assignment was to create one plot that will answer each question. I
# choose "bluntly disregard" that task :). Feel free to deduct the points. But I
# felt that it was also important to show the way I was approaching the data,
# why I choose one way over another. I learned a lot of neet tricks in the
# process and wanted to save them for myself as well as show them to the people
# who will grade my project. In general the last graph will answer the question
# at hand. I also will put comments in the code that will summarize fundings.

# I am planning to put all source code and png files on github. 

# There is an additional file Init.R that is sourced in the beginning of each 
# file. It loads data, instal packages, define functions that are used in more
# than one place.

node <- "/Users/anna/OneDrive/Coursera/JHU - Data Science/"
localPath <- "4. Exploratory Data Analysis/Week 03/Project 2/"
setwd(paste(node, localPath, sep = ""))
source("Init.R")


# Build plots
if (file.exists("plot1.png"))
      file.remove("plot1.png")
png(filename = "plot1.png", width = 400, height = 800)

par(mfrow = c(3, 1))

# regular boxplot with outliers
boxplot(Emissions ~ year, NEI, outline = TRUE, 
        ylab = "Emission", xlab = "", main = "Outliers present")
mtext("USA data")
legend("topleft", bty = "n",
       legend = c("Large number of outliers in the",
                  "data makes it difficult to see",
                  "the overall trend by year"))

# regular boxplot with outliers removed
boxplot(Emissions ~ year, NEI, outline = FALSE, 
          ylab = "Emission", xlab = "", main = "Outliers removed")
legend("topright", bty = "n",
       legend = c("Removing outliers from data makes overall trend",
                  "more obvious but might be misleading"))

# Group data by year, calculate total, mean and median for each year
NEI_byyears <- group_by(NEI, year)
summary <- summarize(NEI_byyears, totalEmission = sum(Emissions, na.rm = TRUE),
                     mean = mean(Emissions, na.rm = TRUE),
                     median = median(Emissions, na.rm = TRUE))
print (summary)

# Scale numerical data, so it will make sense on the same plot
summary <- mutate(summary, scaledEmission = totalEmission / 1000000, 
                  scaledMedian = median * 100)

# plot summary data
plot(summary$year, summary$mean, col = "black", type = "b", xlab = "", 
     main = "Scaled Emission Data by Year",
     ylab = "Scaled Emission Values", ylim = c(0.5, 7.5))
lines(summary$year, summary$scaledMedian, col = "red", type = "b")
lines(summary$year, summary$scaledEmission, col = "blue", type = "b")
legend("top", bty = "n",
       legend = c("Plotting values for scaled total,",
                  "mean and median values make overall",
                  "trend clear"))
legend("topright", lty = 1, lwd = 2,  col = c("blue", "black", "red"), 
       legend = c("Total Emission", "Mean Emission", "Median Emission"))

dev.off()