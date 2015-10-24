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

# getSCC_Rows will check term1 and term2 are in the colName of SCC.
# If getAll = False -> list of unique rows from SCC with term1 and term2 is 
# returned. 
# If getAll = True -> list of all rows from SCC with term1 and term2 is 
# returned. 
getSCC_Rows <- function ( colName, term1, term2, getAll = TRUE) 
{
      column <- SCC[,colName]
      term1_list <- grep (term1, column, value = TRUE)
      term1and2_list <- grep(term2, term1_list, value = TRUE)
      uniqueList <- unique(term1and2_list)
      if ( getAll)
            SCC %>% filter(column %in% uniqueList)
      else
            uniqueList
}

# load packages that I will be using
pkgTest("dplyr")
library(dplyr)
pkgTest("tidyr")
library(tidyr)
pkgTest("ggplot2")
library(ggplot2)
pkgTest("lattice")
library(lattice)
pkgTest("grid")
library(grid)

# load data
NEI <- readRDS("NEI_data/summarySCC_PM25.rds")
# check that data has only years that we need:
NEI %>% select( year ) %>% distinct()

SCC <- readRDS("NEI_data/Source_Classification_Code.rds")

NEI_Baltimore <- NEI %>% filter (fips == "24510")

NEI_BandLA <- NEI %>% filter ( fips == "06037" | fips == "24510")










