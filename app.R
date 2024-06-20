#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(here)
library(shiny)
library(bslib)
library(sf)
library(leaflet)

services_standard <- read.csv(here("data", "SanDiego_Demo_Services.csv"))

# Define UI
ui <- page_sidebar(
  titlePanel("DEMO - HIV Care Locations"),
  sidebar = sidebar(
    open = "open",
    radioButtons("mobile", HTML("<b>Site type:</b>"),
                 choices = list("All" = "All", "Mobile" = "Yes", "Not mobile" = "No"),
                 selected = "All"),
    checkboxGroupInput("services", HTML("<b>Available services:</b>"),
            choices = setNames(services_standard$ID, services_standard$Name))
  ),
  leafletOutput("mapTestingSites")
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {  
  # Get testing site data
  testing_sites <- read.csv(here("data", "SanDiego_Demo_Locations.csv"))  
  
  output$mapTestingSites <- renderLeaflet({
    if (is.null(input$services)) {
      filtered_sites <- testing_sites
    } else {
      filtered_sites <- testing_sites[testing_sites$StandardServices %in% input$services, ]
    }
    
    # Whether site is mobile site
    if (input$mobile != "All") {
      filtered_sites <- filtered_sites[filtered_sites$Mobile == input$mobile, ]
    }
    
    # Create the map with filtered sites
    leaflet(filtered_sites) %>%
      addProviderTiles("CartoDB.Positron") %>%
      addCircleMarkers(
        lng = ~Longitude, lat = ~Latitude,
        popup = ~Name,                 
        fillColor = ifelse(filtered_sites$Mobile == "Yes", "goldenrod", "darkblue"),
        fillOpacity = 1,
        stroke = FALSE
      )
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
