# You also can find all source code and output files in my github repository:
# https://github.com/apiva57/Coursera_JHU_4EDA_project_2.git

# Remarks for this and all following questions:

# 1. There is an additional file Init.R that is sourced in the beginning of each 
# file. It loads data, instals packages, defines functions that are used in more
# than one place.

# 2. Project assignment was to create one plot that will answer each question. I
# choose to "bluntly disregard" that task :). Feel free to deduct the points.
# But I felt that it was also important to show the way I was approaching the
# data, why I choose one way over another. I learned a lot of neet tricks in the
# process and wanted to save them for myself as well as show them to the people
# who will grade my project.  I will put comments in the code that will
# summarize fundings. There are some explanatory remarks on graphs
# themselves (space permitting).

# Q 1
# Have total emissions from PM2.5 decreased in the United States from 1999 to
# 2008? Using the base plotting system, make a plot showing the total PM2.5
# emission from all sources for each of the years 1999, 2002, 2005, and 2008. 

# Answer 
# Q1 dealing with data that is heavily skewed near zero with a
# few really big values. While analyzing big values it becomes clear that they
# are not outliers (for example values correspoinding to wild forest fires).
# Using boxplot with all data tells us nothing due to how heavy data skewed
# towards zero. Removing obvious outliers using R functionality helps and makes
# it more visual but can be very misleading. So, finally I opted to summarize
# data by year and plot total by year. I think although question was about total
# data, it will be very benefitial to analize mean and median. Because those
# parameters are differ in order of magnitude, it is diffictult to put them on
# the same graph and have a clear understanding of what is going on without
# either using logarithmic scale for total values or lineraly adjusting all
# values to fell into roughly the same range. I selected the second option. I
# also removed numeric reference to numbers from y axes to  clearly show that it
# is a trend only, not real numeric value.

node <- "/Users/anna/OneDrive/Coursera/JHU - Data Science/"
localPath <- "4. Exploratory Data Analysis/Week 03/Coursera_JHU_4EDA_project_2/"
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
     main = "Trend for Total/Median/Mean Emission Data by Year",
     ylab = "Scaled Emission Values", ylim = c(0.5, 7.5), yaxt='n')
lines(summary$year, summary$scaledMedian, col = "red", type = "b")
lines(summary$year, summary$scaledEmission, col = "blue", type = "b")
legend("top", bty = "n",
       legend = c("Plotting values for scaled total,",
                  "mean and median values makes overall",
                  "trend clear"))
legend("topright", lty = 1, lwd = 2,  col = c("blue", "black", "red"), 
       legend = c("Total Emission", "Mean Emission", "Median Emission"))

dev.off()