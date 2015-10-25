## Intro
This repo is an implementation of project 2 for Coursera JHU Exploratory Data Analysis class. You can find description of assignment [here](https://class.coursera.org/exdata-033/human_grading/view/courses/975128/assessments/4/submissions)

Data for the pjoject is available [here](https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip)

There are six questions in the project. I need to create a separate R file with a name plotN.R that will create plotN.png file that will answer each question.

## Files
1. There are 6 R files: plotN.R
2. There is also Init.R file that is sourced from each of the PlotN.R files. It loads data, instals packages, defines functions that are used in more than one place.
3. There are 6 output files plotN.png
4. README.md file

##  Overall approach
Project assignment was to create one plot that will answer each question. I
choose "bluntly disregard" that task :). But I
felt that it was also important to show the way I was approaching the data,
why I choose one way over another. I learned a lot of neet tricks in the
process and wanted to save them for myself as well as show them to the people
who will grade my project. I will put comments in the code that will summarize fundings. There will be some explanatory remarks on graphs themselves (space permitting).

## Detailed notes
### Q1 and Q2
Both question dealing with data that is heavily skewed near zero with a few really big values. While analyzing big values it becomes clear that they are not outliers (for example values correspoinding to wild forest fires). Using boxplot with all data tells us nothing due to how heavy data skewed towards zero. Removing obvious outliers using R functionality helps and makes it more visual but can be very misleading. So, finally I opted to summarize data by year and plot total by year. I think although question was about total data, it will be very benefitial to analize mean and median. Because those parameters are differ in order of magnitude, it is diffictult to put them on the same graph and have a clear understanding of what is going on without either using logarithmic scale for total values or lineraly adjusting all values to fell into roughly the same range. I selected the second option. I also removed numeric reference to numbers from y axes to  clearly show that it is a trend only, not real numeric value.

### Q3
First 4 plots were mainly a way to learn a lesson that:

1. graphs can be very misleading depending on how you choose to subset the data
2. logarithmic scale can be very powerful way to show trends in data with a kind of distribution we have
3. dealing with logarithmic scale and zeroes in the data can be tricky and I am not sure I know what is the right approach
4. trying to use smoothing with linear regression (`geom_smooth(method = "lm")`) over essentially 4 series of data is not a good idea.  
  
Plot 5 is showing scaled relationship between total/median/mean emission for  each type. It is quite busy.   
I will call plot 6 here as a main answer, it showes total emission per type.  
Plots 7 and 8 are showing Median and Men Emissions.
 
 
### Q4
The main difficulty here was to decide how to select data. I ended up looking for words "Coal" and "Comb" in every column of SCC data. If I will find those 2 words in any of the columns than I will include that SCC code into the data that I am plotting.

Plots are quite boring. The first plot is a main answer and it showes changes in the Total Emission from Coal Combustion souces by year. Second and third plot solidify the trend from the first plot with very similar trend for Median and Mean values. 

### Q5
I approached this question in the same way as a question 4.  It is interesting to see that trends for Total Emission and Mean Emission are quite similar. But trend for median emission is very different. I am not sure what to think of it. I do know that I have only 88 records in Baltimore to summarize, maybe it is not large enough sample to have similar trend for all three variables.
 
 
 