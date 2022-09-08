#' Author: Rahul Ghosh
#' Date: 09-30-2021
#' Purpose: OKCupid Case 
#' Start Script 
install.packages('okcupiddata')
install.packages('dplyr')
install.packages('mosaic')
install.packages('DataExplorer')
library(okcupiddata)
library(dplyr)
library(DataExplorer)
library(radiant.data)
library(ggplot2)

# Set WD
setwd("~/Data Mining HES 2021/Harvard_DataMining_Business_Student/Cases/I Ok Cupid")

# See all files in wd, leave this blank inside the parentheses
dir()

# Get the okcupid data as `profiles`
profiles <- read.csv('profiles.csv')
address <- read.csv('addr.csv')
latlong <- read.csv('LatLon.csv')


#ORIENTATION
table(profiles$orientation)

#Relationship Between Sex and Sexual Orientation - USE THIS AS ONE OF THE ITEMS 
s.orientation <- tally(~sex + orientation, data=profiles)
s.orientation
mosaicplot(s.orientation, main="Sex vs Orientation", las=1)

#AGE DISTRIBUTION OF OKCUPID MEMBERS - USING THIS 
hist(profiles$age, main="Age Distribution", xlab = 'Age', xlim = c(0, 100), ylim = c(0,20000), col.main = 'blue', col.lab = 'Red', freq = TRUE )

# Stacked Bar Plot with Colors and Legend - INCOME VS ORIENTATION
counts <- table(profiles$income, profiles$sex)
barplot(counts, main="INCOME VS SEX",
        xlab="SEX", ylab ="INCOME", col=c("blue","red"),
        legend = rownames(counts))
		
##Height DISTRIBUTION OF OKCUPID MEMBERS
require(dplyr)
profiles.subset <- filter(profiles, height>=50 & height <=80)
histogram(~height | sex, width=1, layout=c(1,2), xlab="Height",
          data=profiles.subset)

histogram(~height | sex, width=1, layout=c(1,2), xlab="Height",
          data=profiles.subset)


#' END Script 
