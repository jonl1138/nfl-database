library("dplyr")
library("Quandl")
library("httr")
library("jsonlite")
library("ggplot2")

source('secret/api-key.R')
## reads in user-given API key to allow access to Quandl API data
Quandl.api_key(api_key)

## reads in various indicator codes to data frames
unfiltered_indicators <- read.csv("indicators.csv", stringsAsFactors = FALSE)
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
                              "Zillow Home Value Index - All Homes",
                              "Zillow Home Value Index - Bottom Tier",
                              "Zillow Home Value Index - Middle Tier",
                              "Zillow Home Value Index - Top Tier")
filtered_indicators <- filteredIndicatorBuilder(desired_indicator_params)

## Takes in a string area category (county, state, city), and returns a factor of all possible choices in
## that category
mappingFinder <- function(area_name) {
  if (area_name == "City") {
    return(city_mapping_code$AREA)
  }
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

## takes in a user input string (name of state, county, or etc.) and returns the matching indicator code
## using the mapping_code dataframes
subCodeBuilderArea <- function(area_name,area_category) {
  if (area_category == 'State') {
    return(as.character(filter(state_mapping_code,AREA == area_name)$CODE))
  }
  else if (area_category == 'County') {
    return(as.character(filter(county_mapping_code,AREA == area_name)$CODE))
  }
  else if (area_category == 'City') {
    return(as.character(filter(city_mapping_code,AREA == area_name)$CODE))
  }
  else if (area_category == 'Greater Metropolitan Area') {
    return(as.character(filter(greatermetro_mapping_code,AREA == area_name)$CODE))
  }
  else if (area_category == 'Neighborhood') {
    return(as.character(filter(neighborhood_mapping_code,AREA == area_name)$CODE))
  }
  else {
    return(area_name)
  }
}

## takes in a string of the specific choice of area, the string of the specific choice of search category, and
## returns the Quandl api code to get the data fitting those parameters
codeBuilder <- function(indicator_name,area_category,area_name) {
  area_abbrev <- 'Z'
  if (area_category == 'State') {
    area_abbrev = 'S'
  } else if (area_category == "County") {
    area_abbrev = "CO"
  } else if (area_category == "City") {
    area_abbrev = "C"
  } else if (area_category == "Greater Metro Area") {
    area_abbrev = "M"
  } else if (area_category == "Neighborhood") {
    area_abbrev = "N"
  }
  return(paste0('ZILLOW/',area_abbrev,subCodeBuilderArea(area_name,area_category),'_',subCodeBuilderIndicator(indicator_name)))
}

## takes in a string "title" of a certain indicator and returns a string of the matching indicator code
subCodeBuilderIndicator <- function(indicator_name) {
  return(as.character(filter(filtered_indicators,INDICATOR == indicator_name)$CODE))
}

data3 <- Quandl('ZILLOW/S3_ZHVIAH')
filtered_data3 <- filter(data3, substring(Date,1,4) == '2018')
data4 <- Quandl('ZILLOW/Z98006_PHDVAH')

ggplot(data = data3) +
  geom_point(mapping = aes(x= data3$Date, y= data3$Value))


