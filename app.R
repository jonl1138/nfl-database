library("dplyr")
library("httr")
library("jsonlite")
library("Quandl")
library("shiny")

source("helper.R")

ui <- fluidPage(
  titlePanel("Real Estate Information"),
  sidebarLayout(
    sidebarPanel(
      ## Dropdown menu to choose which type of area one wants to limit the area search by
      selectInput("area_category", "Search By:", c("Zip Code","County", "City", "State", "Neighborhood", "Greater Metropolitan Area")),
      
      ## dropdown to choose what range of dates to use in displaying information
      dateRangeInput("date_range", label = "Date range", start = "1996-04-30"),
      
      ## outputs the searchbar in the ui
      uiOutput("searchbar2"),
      
      ## dropdown to determine the category of information desired 
      selectInput("indicator",
                  label = "Desired Parameter",
                  c("Choose" = '',
                    filtered_indicators$INDICATOR),
                  selectize = TRUE)

      

      
    ),
    mainPanel(
      plotOutput("graph", width = "100%", height = "400px")
    )
  )
)

server <- function(input, output, session) {
  source("helper.R")
  # filtered_data3 <- filter(data3, substring(Date,1,4) == '2018')
  # temp_graph <- ggplot(data = data3) +
  #   geom_point(mapping = aes(x= data3$Date, y= data3$Value))
  # output$graph <- renderPlot(temp_graph)
  
  output$graph <- ggplot
  
  ## renders the search bar for choosing the specific area name 
  output$searchbar2 <- renderUI(selectInput('area_input', 
                                            'Enter Specific Area Name', 
                                            c("Choose" ='',as.character(mappingFinder(input$area_category))), 
                                            selectize=TRUE)
)
  
  
  

}

shinyApp(ui, server)