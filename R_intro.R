# clear environment
rm(list=ls(all=TRUE))

# set the working directory
setwd("C:/Users/choy9/OneDrive/Desktop/School/Summer20/Data Science/Module 3")

# basic math
7+4 # addition
7-4 # subtraction
7*4 # multiplication
7/4 # division

## Vectors, Sequences, and Data Classes/Types

# creating a country character vector
country <- c("france","france", "france", "france", "france", "france")

#create a vector for years
year = c(2000, 2005, 2010, 2015, 2020, 2025)

# create a year variable via a sequence
year2 <- seq(2000,2025,5)
rm(year2)

# inspect the year vector
print(year)

# poverty rate
poverty_rate <- c(13.6, 13.1, 14, 14.2,NA,NA)

# GDP per capita
#consumption (C) + Income (I) + Government Expenditures (G) + (Exports - Imports)
# C + I + G + (X - M) / population
gdp_per_capita <- c(22364, 34760, 40368, 36613,NA,NA)

# classify the GDP by low or high
low_high <- c("low", "low", "high", "high", "NA", "NA")

# create a factor variable
# so R knows that low is less than high
gdp_levels <- factor(low_high, levels = c("low", "high"))

# remove the low_high string variable
rm(low_high)

# check the class/type of our vectors
class(country)
class(poverty_rate)
class(gdp_per_capita)
class(gdp_levels)
class(year)

## Dataframes, variables, Basic Statistics, and Missing Data

# convert vectors to data frame
df <- data.frame(country, year, poverty_rate, gdp_per_capita, gdp_levels)

# remove vector, so we just have a data frame
rm(country, gdp_levels, gdp_per_capita, poverty_rate, year)

# inspect the data frame
View(df)
head(df)
summary(df)
dim(df)

# inspect the variables individually
summary(df$gdp_per_capita)
summary(df$poverty_rate)

# count the observations
length(df$poverty_rate)

# count the number of unique observations
length(unique(df$poverty_rate))

# check the class of the country variable
class(df$country)

# change country back to a character
df$country <- as.character(df$country)
class(df$country)
rm(df$country_char)

# get descriptive statistics of our variables
mean(df$gdp_per_capita)
mean(df$gdp_per_capita, na.rm = TRUE)

# assign the mean gdp per capita as another variable in the df
mean_gdp <- mean(df$gdp_per_capita, na.rm = TRUE)
print(mean_gdp)

#correlation between poverty rate and GDP per capita
cor(df$gdp_per_capita, df$poverty_rate, use = "complete.obs")

## Installing Packages, Making Tables with Stargazer, and Getting Help

# install stargazer
# can use install.packages("stargazer")

# load the stargazer library
library(stargazer)

# output our dataset with some descriptive stats with stargazer
stargazer(df, type = "text")

# help function for stargazer
?stargazer

## Importing Data and Intro to the V-dem Dataset

#import V-dem dataset
library(rio)
vdem <- import("V-Dem-CY-Core-v10.dta")

# which countries are included in V-Dem?
table(vdem$country_name)

# which years are in V-dem?
summary(vdem$year)

## Subsetting and Removing Data Frames

# subset vdem
vdem2 <- subset(vdem, select = c("country_name", "year",
                                 "COWcode", "v2x_polyarchy",
                                 "v2x_corr"))

# na.omit on the basis of democracy and corruption
vdem3 <- na.omit(vdem2, select = c("v2x_polyarchy",
                                   "v2x_corr"))

# correlation between democracy and corruption
cor(vdem3$v2x_polyarchy, vdem3$v2x_corr)

## Creating New Variables and Cross-Tabs

# get the mean democracy score
mean(vdem3$v2x_polyarchy)

# create a new variable democracy dummy variable (0 or 1)
vdem3$democracy = NA
vdem3$democracy[vdem3$v2x_polyarchy >= .27] = 1
vdem3$democracy[vdem3$v2x_polyarchy < .27] = 0

# create an autocracy variable dummy variable (0 or 1)
vdem3$autocracy = NA
vdem3$autocracy[vdem3$v2x_polyarchy < .27] = 1
vdem3$autocracy[vdem3$v2x_polyarchy >= .27] = 0

# create political regime variable
vdem3$political_regime = "NA"
vdem3$political_regime[vdem3$democracy == 1] = "democracy"
vdem3$political_regime[vdem3$democracy == 0] = "autocracy"

# create political regime variable in another way
vdem3$political_regime = "NA"
vdem3$political_regime[vdem3$autocracy == 1] = "autocracy"
vdem3$political_regime[vdem3$autocracy == 0] = "democracy"

# table the political regime
table(vdem3$political_regime, exclude = TRUE)

# run the cross tab
library(doBy)
summaryBy(v2x_corr ~ political_regime, data = vdem3, FUN = c(mean, length))      

## Saving/Export

?rio
export(vdem3, "new_vdem.csv") # csv file
export(vdem3, "new_vdem.dta") # Stata dataset
export(vdem3, "new_vdem.xlsx") # Excel