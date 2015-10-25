node <- "/Users/anna/OneDrive/Coursera/JHU - Data Science/"
localPath <- "4. Exploratory Data Analysis/Week 03/Coursera_JHU_4EDA_project_2/"
setwd(paste(node, localPath, sep = ""))
source("Init.R")

# Q3 
# Of the four types of sources indicated by the type (point, nonpoint, onroad, 
# nonroad) variable, which of these four sources have seen decreases in 
# emissions from 1999–2008 for Baltimore City? Which have seen increases in 
# emissions from 1999–2008? Use the ggplot2 plotting system to make a plot 
# answer this question

# Answer
# First 4 plots were mainly a way to learn a lesson that:
# - graphs can be very misleading depending on how you choose to subset the data
# - logarithmic scale can be very powerful way to show trends in data with a
#   kind of distribution we have
# - dealing with logarithmic scale and zeroes in the data can be tricky and I
#   am not sure I know what is the right approach
# - trying to use smoothing with linear regression (geom_smooth(method = "lm"))
#   over essentially 4 series of data is not a good idea.
# Plot 5 is showing scaled relationship between total/median/mean emission for 
# each type. It is quite busy. 
# I will call plot 6 here as a main answer, it showes total emission per type.
# Plots 6 and 7 are showing Median and Men Emissions.

# multiplot function allowes to print multiple ggplot graphs on the same page.
# ( it is modified version of this example: 
# http://www.cookbook-r.com/Graphs/Multiple_graphs_on_one_page_(ggplot2)/)
# ggplot objects can be passed in ..., 
# cols:   Number of columns in layout
multiplot <- function(..., cols=1) {
  # Make a list from the ... arguments 
  plots <- c(list(...))
  numPlots = length(plots)
  
  # Make the panel
  # ncol: Number of columns of plots
  # nrow: Number of rows needed, calculated from # of cols
  layout <- matrix(seq(1, cols * ceiling(numPlots/cols)), byrow = TRUE,
                     ncol = cols, nrow = ceiling(numPlots/cols))
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

# let's get overall feel for data, this code is purely for getting feel of the
# data
summary(NEI_Baltimore)
unique(NEI_Baltimore$year)

summary(NEI_Baltimore[NEI_Baltimore$year == 1999,])
summary(NEI_Baltimore[NEI_Baltimore$year == 2002,])
summary(NEI_Baltimore[NEI_Baltimore$year == 2005,])
summary(NEI_Baltimore[NEI_Baltimore$year == 2008,])

tapply(NEI_Baltimore$Emissions, NEI_Baltimore$year, IQR)
tapply(NEI_Baltimore$Emissions, NEI_Baltimore$year, fivenum)

sum(NEI_Baltimore$Emissions == 0)
sum(NEI_Baltimore[NEI_Baltimore$year == 1999,]$Emissions == 0)
sum(NEI_Baltimore[NEI_Baltimore$year == 2002,]$Emissions == 0)
sum(NEI_Baltimore[NEI_Baltimore$year == 2005,]$Emissions == 0)
sum(NEI_Baltimore[NEI_Baltimore$year == 2008,]$Emissions == 0)


# start plotting
png(filename = "plot3.png", width = 1200, height = 2400)

# Attempt 0 - I know that data are very dence between 0 and 5, but also have 
# a few really big values. First, I want to use a regualr boxplot  to get 
# a feel for outliers
# gm1 <- ggplot(NEI_Baltimore, aes(type, Emissions)) +
#       labs(title = "Emission in Baltimore by type") + 
#       geom_boxplot()
# 
# let's drop all the values that are greater than 100. After that we can see that
# max non-outlier value for NONPOINT data is about 40. So I would consider all 
# values that are less than 40. Repeating the same logic multiple times I decided to 
# stop at 12
NEI_Baltimore_NoOutliers <- filter(NEI_Baltimore, Emissions <= 12 )
# how many records I dropped?
n <- dim (NEI_Baltimore) - dim(NEI_Baltimore_NoOutliers)
# or in percentage:
pct <- n/nrow(NEI_Baltimore)*100
# g0 <- ggplot(NEI_Baltimore_NoOutliers, aes(type, Emissions)) +
#   labs(title = "Emission in Baltimore by type\n(113 outliers records are dropped or ~5.4%)") + 
#   geom_boxplot()

# Scenario 1 - plain vanilla
g1 <- ggplot(NEI_Baltimore, aes(year, Emissions)) +
  labs(title = 
  "Emission in Baltimore by type, all data is used\nhard to see any trend yet") + 
  geom_point() + 
  geom_smooth(method = "lm") + 
  scale_x_continuous(breaks=seq(1999, 2008, 3), 
                     label=c("99", "02", "05", "08") ) +
  theme(panel.grid.minor.x=element_blank()) +
  xlab("Year") + 
  facet_grid(. ~ type)

# Scenario 2 - disregard outliers
g2 <- ggplot(NEI_Baltimore_NoOutliers, aes(year, Emissions)) +
  labs(title = 
  "Emission in Baltimore by type \n(113 outliers records are dropped or ~5.4%)") + 
  geom_point() + 
  geom_smooth(method = "lm") + 
  scale_x_continuous(breaks=seq(1999, 2008, 3), 
                     label=c("99", "02", "05", "08") ) +
  theme(panel.grid.minor.x=element_blank()) +
  xlab("Year") +
  facet_grid(. ~ type)

# Scenario 3 - disregard 0 and use log(Emission)
head(NEI_Baltimore %>% arrange(Emissions), 50)
tail(NEI_Baltimore %>% arrange(Emissions), 50)

zeroes <- filter(NEI_Baltimore, Emissions == 0)
str(zeroes)
128/nrow(NEI_Baltimore)*100

NEI_Baltimore_nonzeroes <- filter(NEI_Baltimore, Emissions > 0) %>% 
  mutate(Emissions = log(Emissions))

head(NEI_Baltimore_nonzeroes %>% arrange(Emissions), 50)
tail(NEI_Baltimore_nonzeroes %>% arrange(Emissions), 50)

ttl1 <- "Logariphmic Value of Emission in Baltimore by type\n"
ttl2 <- "(128 zero values are dropped or ~6.1%)"
g3 <- ggplot(NEI_Baltimore_nonzeroes, aes(year, Emissions)) +
  labs(title = paste(ttl1, ttl2, sep = "") ) + 
  geom_point() + 
  geom_smooth(method = "lm") + 
  facet_grid(. ~ type) + 
  scale_x_continuous(breaks=seq(1999, 2008, 3), 
                     label=c("99", "02", "05", "08") ) +
  theme(panel.grid.minor.x=element_blank()) +
  xlab("Year") +
  ylab("Logariphm of Emissions")

# Scenario - 4
# I am not happy that I had to drop either large values or zero values in order 
# to discover a trend. I want to find a way to use all data. Scenario 4 -
# multiply Emission by 10^8 ( the minimal non-zero emission is 4.421016e-07 ) 
# set emission to 1 where it is 0. Use log values ( 10-based) for Emission.

NEI_Baltimore <- NEI_Baltimore %>% mutate(modEmissions = 0) 
nonZero <- NEI_Baltimore %>% filter(Emissions > 0) %>% 
  mutate(modEmissions = log10(Emissions*10^8))
zero <- NEI_Baltimore %>% filter(Emissions == 0) %>%
  mutate(modEmissions = log10(1))
NEI_Baltimore <- rbind(zero, nonZero)

str(NEI_Baltimore)
head(NEI_Baltimore %>% arrange(modEmissions), 50)
tail(NEI_Baltimore %>% arrange(modEmissions), 50)

ttl1 <- "Emission in Baltimore by type\n"
ttl2 <- "(10-based log value of Emission multiplied by 10^8,\n0 values set to 1)"
g4 <- ggplot(NEI_Baltimore, aes(year, modEmissions, 10)) +
    labs(title = paste(ttl1, ttl2, sep = "")) + 
    geom_point() + 
    geom_smooth(method = "lm") + 
    facet_grid(. ~ type) + 
    scale_x_continuous(breaks=seq(1999, 2008, 3), 
                       label=c("99", "02", "05", "08") ) +
    theme(panel.grid.minor.x=element_blank()) +
    xlab("Year") +
    ylab("10-base logariphm of modified Emisssions")

# Scenario - 5
# Now trend for Non-Road contradicts with my previous discoveries. Before I 
# always saw overall decreasing trend for each type. But now Non-Road trend is 
# increasing. I think that due to nature of our data using mean values over 4
# points (years) for a trend maybe misleading ( and mean values are used to for
# geom_smooth(method = "lm"). I want to look at sum, mean and median values for 
# each year in order to judge a trend.

# Let's calculate median/mean/total values per year per type first.
raw_totals <- NEI_Baltimore  %>% 
  group_by(type, year)  %>% 
  summarise_each(funs(mean, median, sum), Emissions)
# Values are span from 0.009 to 2107. In order to be able to visualize it better
# I'll scale each type/variable group separately to get to all numbers roughly into 
# 0 - 12 range
nonroad <- raw_totals %>% 
  filter ( type == "NON-ROAD") %>% 
  mutate ( mean = mean*2, median = median*100, sum = sum / 50)
nonpoint <- raw_totals %>% 
  filter ( type == "NONPOINT") %>% 
  mutate ( mean = mean / 10, sum = sum / 200)
onroad <- raw_totals %>% 
  filter ( type == "ON-ROAD") %>% 
  mutate ( mean = mean*5, median = median*50, sum = sum / 30)
point <- raw_totals %>% 
  filter ( type == "POINT") %>% 
  mutate ( mean = mean/2, sum = sum / 100)
#Let's assemble it back together and make it tidy
raw_totals_scaled <- rbind(nonroad, nonpoint, onroad, point )
raw_totals_scaled <- gather(raw_totals_scaled, 
                            function_name, num_value, mean:sum )

g5 <- ggplot(raw_totals_scaled, 
             aes(x=year, y=num_value, group=function_name, color=function_name)) +
  geom_line() + 
  facet_grid(. ~ type) +
  ylab("Mean / Median / Sum -  All scaling is done separetly per type/function") +
  scale_x_continuous(breaks=seq(1999, 2008, 3), 
                     label=c("99", "02", "05", "08") ) +
  theme(panel.grid.minor.x=element_blank()) +
  xlab("Year") +
  theme(legend.justification=c(0,0), legend.position=c(0,0)) +
  scale_color_discrete(name  ="Function",
                       breaks=c("mean", "median", "sum"),
                       labels=c("Mean", "Median", "Sum")) +
  ggtitle("Scaled Mean / Median / Sum")

# Scenarios - 6 throghw 8. 
# After all the troubles to build previous graph, I think it is way too busy
# and all the scaling makes it difficult to  understand trends. So I will build
# 3 more boring line plots for Total, Mean and Median Emission. I still have all
# data I need for that in raw_totals
g6 <- ggplot(raw_totals, aes(x=year, y=sum)) +
      geom_line(color = "blue", size = 1.25) + 
      geom_point(color = "blue", size = 3) +
      facet_grid(. ~ type) +
      ylab("Total Emissions") +
      scale_x_continuous(breaks=seq(1999, 2008, 3), 
                         label=c("99", "02", "05", "08") ) +
      theme(panel.grid.minor.x=element_blank()) +
      xlab("Year") +
      ggtitle("Total Emissions")
g7 <- ggplot(raw_totals, aes(x=year, y=median)) +
      geom_line(color = "green", size = 1.25) + 
      geom_point(color = "green", size = 3) +
      facet_grid(. ~ type) +
      ylab("Median Emissions") +
      scale_x_continuous(breaks=seq(1999, 2008, 3), 
                         label=c("99", "02", "05", "08") ) +
      theme(panel.grid.minor.x=element_blank()) +
      xlab("Year") +
      ggtitle("Median Emissions")
g8 <- ggplot(raw_totals, aes(x=year, y=mean)) +
      geom_line(color = "red", size = 1.25) + 
      geom_point(color = "red", size = 3) +
      facet_grid(. ~ type) +
      ylab("Mean Emissions") +
      scale_x_continuous(breaks=seq(1999, 2008, 3), 
                         label=c("99", "02", "05", "08") ) +
      theme(panel.grid.minor.x=element_blank()) +
      xlab("Year") +
      ggtitle("Mean Emissions")
multiplot(g1, g2, g3, g4, g5, g6, g7, g8, cols=2)
dev.off()

# I modified NEI_Baltimore in this file. Will cleanup it later. For now I will 
# just remove it from the environment
rm(NEI_Baltimore)
