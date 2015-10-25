# Define Functions:

# pkgTest will check if package is installed and install it if needed
pkgTest <- function(x)
{
      if (!require(x,character.only = TRUE))
      {
            install.packages(x,dep=TRUE)
            if(!require(x,character.only = TRUE)) stop("Package not found")
      }
}

# getSCC_Rows will check word1 and word2 are in the colName of SCC.
# If getAll = False -> list of unique rows from SCC with word1 and word2 is 
# returned. 
# If getAll = True -> list of all rows from SCC with word1 and word2 is 
# returned. 
getSCC_Rows <- function ( colName, word1, word2, getAll = TRUE) 
{
      column <- SCC[,colName]
      word1_list <- grep (word1, column, value = TRUE)
      word1and2_list <- grep(word2, word1_list, value = TRUE)
      uniqueList <- unique(word1and2_list)
      if ( getAll)
            SCC %>% filter(column %in% uniqueList)
      else
            uniqueList
}

# load packages that I will be using
print ("Start loading required packages ...")
pkgs <- c("dplyr", "tidyr", "ggplot2", "lattice", "grid")
sapply(pkgs, pkgTest)

library(dplyr)
library(tidyr)
library(ggplot2)
library(lattice)
library(grid)
print ("... done loading required packages.")

# load data
print ("Start loading data ...")
if(!exists("NEI")){
      NEI <- readRDS("./NEI_data/summarySCC_PM25.rds")
}

if(!exists("SCC")){
      SCC <- readRDS("./NEI_data/Source_Classification_Code.rds")
}

if(!exists("NEI_Baltimore")){
      NEI_Baltimore <- NEI %>% filter (fips == "24510")
}

if(!exists("NEI_BandLA")){
      NEI_BandLA <- NEI %>% filter ( fips == "06037" | fips == "24510")
}
print ("... done loading data.")
# just checking that data has only years that we need:
NEI %>% select( year ) %>% distinct()








