node <- "/Users/anna/OneDrive/Coursera/JHU - Data Science/"
localPath <- "4. Exploratory Data Analysis/Week 03/Coursera_JHU_4EDA_project_2/"
setwd(paste(node, localPath, sep = ""))
source("Init.R")

# Q2 
# Have total emissions from PM2.5 decreased in the Baltimore City, Maryland
# (fips == "24510") from 1999 to 2008? Use the base plotting system to make a
# plot answering this question. 

# Answer 
# I used the same approach as with Question1

# Build plots
if (file.exists("plot2.png"))
      file.remove("plot2.png")
png(filename = "plot2.png", width = 400, height = 800)
par(mfrow = c(3, 1))

# regular boxplot with outliers
boxplot(Emissions ~ year, NEI_Baltimore, outline = TRUE, 
        ylab = "Emission", xlab = "", main = "Outliers present")
mtext("Baltimore data", adj = 0)
legend("topright", bty = "n",
       legend = c("Large number of outliers in the",
                  "data makes it difficult to see",
                  "the overall trend by year"))

# regular boxplot with outliers removed
boxplot(Emissions ~ year, NEI_Baltimore, outline = FALSE, 
          ylab = "Emission", xlab = "", main = "Outliers removed")
legend("topright", bty = "n",
       legend = c("Removing outliers from data makes overall trend",
                  "more obvious but might be misleading"))

# Group data by year, calculate total, mean and median for each year
NEIB_byyears <- group_by(NEI_Baltimore, year)
summary <- summarize(NEIB_byyears, totalEmission = sum(Emissions, na.rm = TRUE),
                     mean = mean(Emissions, na.rm = TRUE),
                     median = median(Emissions, na.rm = TRUE))


# Scale numerical data, so it will make sense on the same plot
summary <- mutate(summary, scaledEmission = totalEmission / 250, 
                  scaledMedian = median * 20)
print (summary)
# plot summary data
plot(summary$year, summary$mean, col = "black", type = "b", xlab = "", 
     main = "Trend for Total/Median/Mean Emission Data by Year",
     ylab = "Scaled Emission Values", ylim = c(0.0, 16.0), yaxt='n')
lines(summary$year, summary$scaledMedian, col = "red", type = "b")
lines(summary$year, summary$scaledEmission, col = "blue", type = "b")
legend("top", bty = "n",
       legend = c("Plotting values for scaled total,",
                  "mean and median values make",
                  "overall trend clear"))
legend("topright", lty = 1, lwd = 2,  col = c("blue", "black", "red"), 
       legend = c("Total Emission", "Mean Emission", "Median Emission"))

dev.off()