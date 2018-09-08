library("dplyr")
library("Quandl")
library("ggplot2")
library("stringr")

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

## reads in csv files linking State abbreviations to their full names
state_abbrev_code <- read.csv("us_states.csv", stringsAsFactors = FALSE)

## takes in a vector of desired Zillow parameters and filters the indicators.csv file to write a new .csv
## (filtered_indicators.csv) containing only the indicator codes of said desired parameters
filteredIndicatorBuilder <- function(parameters) {
  df <- unfiltered_indicators %>%
    filter(INDICATOR %in% parameters)
  write.csv(df, file = "filtered_indicators.csv")
  return(df)
}

## vector to be used as the input in filteredIndicatorBuilder
desired_indicator_params <- c("Median Sold Price - All Homes",
                              "Median Sold Price Per Square Foot - All Homes",
                              "Percent Of Homes Decreasing In Values - All Homes",
                              "Percent Of Homes Increasing In Values - All Homes",
                              "Zillow Home Value Index - All Homes",
                              "Zillow Home Value Index - Bottom Tier",
                              "Zillow Home Value Index - Middle Tier",
                              "Zillow Home Value Index - Top Tier")
filtered_indicators <- filteredIndicatorBuilder(desired_indicator_params)

## takes in a string name of a city/neighborhood/greatermetro, a string name of an indicator, and a dataframe of
## an already processed query of the specific area name and returns a string comparing the specific area with the
## state in question in regards to the chosen indicator
comparisonStringBuilder <- function(area_name,indicator_name, df, date_range) {
  state_abbrev <- str_sub(area_name, -2, -1)
  state_name <- filter(state_abbrev_code, ABBREV == state_abbrev)$STATE
  state_df <- filter(
    Quandl(codeBuilder(indicator_name,"State",state_name))
  , Date > date_range[1] & Date < date_range[2]
  )
  if (state_df[nrow(state_df),2] > df[nrow(df),2]) {
      phrase <- "is less than the statewide measurement of "
  } else if (state_df[nrow(state_df),2] == df[nrow(df),2]){
      phrase <- " is equal to the statewide measurement of "
  } else {
      phrase <- " is greater than the statewide measurement of "
  }
  return(paste0("-- ",
                "The latest Zillow measurement of the ",
                indicator_name,
                " of ",
                area_name,
                " ",
                phrase,
                " ",
                state_name,
                " (",
                area_name,
                " recorded ",
                df[nrow(df),2],
                " on ",
                state_df[nrow(state_df),1],
                " versus ",
                state_name,
                " recording ",
                state_df[nrow(state_df),2],
                " on ",
                state_df[nrow(state_df),1],
                ")"
                )
  )
}

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

## takes in a string "title" of a certain indicator and returns a string of the matching indicator code
subCodeBuilderIndicator <- function(indicator_name) {
  return(as.character(filter(filtered_indicators,INDICATOR == indicator_name)$CODE))
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