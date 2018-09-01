library("dplyr")
library("httr")
library("jsonlite")
library("Quandl")
library("shiny")

source("helper.R")

ui <- fluidPage(
  titlePanel("Real Estate Around You"),
  sidebarLayout(
    sidebarPanel(
      ## Dropdown menu to choose which type of area one wants to limit the area search by
      selectInput("area_category", 
                  "Search By:", 
                  selected = "State",
                  c("Zip Code","County", "City", "State", "Neighborhood", "Greater Metropolitan Area")),
      
      ## dropdown to choose what range of dates to use in displaying information
      dateRangeInput("date_range", 
                     label = "Date range", 
                     start = "2002-04-30"),
      
      ## outputs the specific area searchbar in the ui
      uiOutput("searchbar"),
      
      ## dropdown to determine the category of information desired 
      selectInput("indicator",
                  label = "Desired Parameter",
                  c("Choose" = '',
                   filtered_indicators$INDICATOR),
                  selected = "Zillow Home Value Index - All Homes",
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
  
  
  
  ## renders the search bar for choosing the specific area name 
  output$searchbar <- renderUI(selectInput('area_input', 
                                            'Enter Specific Area Name',
                                            c("Choose" ='',as.character(mappingFinder(input$area_category))), 
                                            selected = 'Washington',
                                            selectize=TRUE)
                                            )
  ## uses the codeBuilder function from helper.R to keep the Shiny app with an updated Quandl API call and
  ## subsequent data frame
  current_dataframe <- reactive({
    return(
      filter(
        Quandl(
          codeBuilder(
            input$indicator,input$area_category,input$area_input
          )
        )
      , Date > as.Date(input$date_range[1]) & Date < as.Date(input$date_range[2])
      )
    )
  })

  ylabel <- reactive({
     
    if (grepl("Percent",input$indicator)) {
      return("Percentage")
    } 
    return("Dollars")
    
  })
  
  output$graph <- renderPlot(
    ggplot(data = current_dataframe()) +
      geom_smooth(mapping = aes(x = current_dataframe()$Date, y = current_dataframe()$Value), color = 'blue') +
      geom_point(mapping = aes(x = current_dataframe()$Date, y = current_dataframe()$Value), color = 'blue') +
      labs(
        title = paste(input$indicator,"of",input$area_input),
        x = "Date",
        y = ylabel()
      )
  )

}

shinyApp(ui, server)