#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#


#set up for the shiny app
library(tmap)
library(sf)
library(dplyr)
library(shiny)

# read in data
load("dipperobs.RData")

# set tmap mode to interactive
tmap_mode("view")

ui <- fluidPage(
  #App title
  titlePanel("Dipper Observations in Colorado"),
  
  # Add some informational text using and HTML tag (i.e., a level 5 heading)
  h5("In this app you can filter occurrences by year, type of observation, and elevation. You can also click on individual occurrences to view metadata."
  ),
  
  # Sidebar layout
  sidebarLayout(
    # Sidebar panel for widgets that users can interact with
    sidebarPanel(
      # Input: select species shown on map
      checkboxGroupInput(
        inputId = "year",
        label = "Year",
        # these names should match that in the dataset, if they didn't you would use 'choiceNames' and 'choiceValues' like we do for the next widget
        choices = list("2013","2014", "2015","2016","2017","2018","2019","2020","2021","2022","2023"),
        # selected = sets which are selected by default
        selected = c("2013","2014", "2015","2016","2017","2018","2019","2020","2021","2022","2023")
      ),
      
      # Input: Filter points by observation type
      checkboxGroupInput(
        inputId = "basisOfRecord",
        label = "Observation Type",
        choiceNames = list(
          "Human Observation",
          "Preserved Specimen",
          "Machine Observation"
        ),
        choiceValues = list(
          "HUMAN_OBSERVATION",
          "PRESERVED_SPECIMEN",
          "MACHINE_OBSERVATION"
        ),
        selected = c("HUMAN_OBSERVATION",
                     "PRESERVED_SPECIMEN",
                     "MACHINE_OBSERVATION"
        )
      ),
      
      
      # Input: Filter by elevation
      sliderInput(
        inputId = "elevation",
        label = "Elevation",
        min = 1000,
        max = 4500,
        value = c(1000, 4500)
      )
      
    ),
    
    # Main panel for displaying output (our map)
    mainPanel(# Output: interactive tmap object
      tmapOutput("map"))
    
  )
  
)


server <- function(input, output){
  
  # Make a reactive object for the occ data by calling inputIDs to extract the values the user chose
  occ_react <- reactive(
      dipperocc %>%
      filter(year %in% input$year) %>%
      filter(basisOfRecord %in% input$basisOfRecord) %>%
      filter(elevation >= input$elevation[1] &
               elevation <= input$elevation[2])
  )
  
  # Render the map based on our reactive occurrence dataset
  output$map <- renderTmap({
    tm_shape(occ_react()) +
      tm_dots(
        col = "Species",
        size = 0.1,
        palette = "Dark2",
        title = "Species Occurrences",
        popup.vars = c(
          "Species" = "Species",
          "Record Type" = "basisOfRecord",
          "Elevation (m)" = "elevation"
        )
      )
    
    
  })
  
  
  
}

shinyApp(ui, server)
