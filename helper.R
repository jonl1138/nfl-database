library("dplyr")
library("Quandl")
library("httr")
library("jsonlite")
library("ggplot2")

source('secret/api-key.R')
## reads in user-given API key to allow access to Quandl API data
Quandl.api_key(api_key)

## reads in various indicator codes to data frames
unfiltered_indicators <- read.csv("indicators.csv")
state_mapping_code <- read.csv("filtered_state_mapping.csv")
county_mapping_code <- read.csv("filtered_county_mapping.csv")
city_mapping_code <- read.csv("filtered_city_mapping.csv")
greatermetro_mapping_code <- read.csv("filtered_greatermetro_mapping.csv")
neighborhood_mapping_code <- read.csv("filtered_neighborhood_mapping.csv")

## takes in a vector of desired Zillow parameters and filters the indicators.csv file to write a new .csv
## (filtered_indicators.csv) containing only the indicator codes of said desired parameters
filteredIndicatorBuilder <- function(parameters) {
  df <- unfiltered_indicators %>%
    filter(INDICATOR %in% parameters)
  write.csv(df, file = "filtered_indicators.csv")
  return(df)
}

## vector to be used as the input in filteredIndicatorBuilder
desired_indicator_params <- c("Median Sold Price",
                              "Median Sold Price Per Square Foot - All Homes",
                              "Percent Of Homes Decreasing In Values - All Homes",
                              "Percent Of Homes Increasing In Values - All Homes",
                              "Zillow Home Value Index - All Homes")
filtered_indicators <- filteredIndicatorBuilder(desired_indicator_params)

## Takes in a string area category (county, state, city), and returns a factor of all possible choices in
## that category
mappingFinder <- function(area_name) {
  if (area_name == "City")
    return(city_mapping_code$AREA)
  else if (area_name == "State") {
    return(state_mapping_code$AREA)
  }
  else if (area_name == "County") {
    return(county_mapping_code$AREA)
  }
  else if (area_name == "Greater Metropolitan Area"){
    return(greatermetro_mapping_code$AREA)
  }
  else if (area_name == "Neighborhood") {
    return(neighborhood_mapping_code$AREA)
  }
}

## takes in a string of the specific choice of area, the string of the specific choice of search category, and
## returns the Quandl api code to get the data fitting those parameters
codeBuilder <- function()


data3 <- Quandl('ZILLOW/Z98006_MLPAH')
filtered_data3 <- filter(data3, substring(Date,1,4) == '2018')
data4 <- Quandl('ZILLOW/Z98006_PHDVAH')

ggplot(data = data3) +
  geom_point(mapping = aes(x= data3$Date, y= data3$Value))


