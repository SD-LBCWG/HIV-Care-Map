#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(here)
library(tidyverse)
library(shiny)
library(leaflet)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("San Diego Demo Map"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("bins",
                        "Number of bins:",
                        min = 1,
                        max = 50,
                        value = 30)
        ),

        # Show map of San Diego zipcodes and testing sites
        mainPanel(
           leafletOutput("mapTestingSites", height = 700)
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  # San Diego zipcode shapefile
  sd_zip_shp <- sf::st_read(here("shapefile", "geo_sdcounty.shp"))
  
  # Get testing site data
  testing_sites <- read.csv(here("data", "SanDiego_Testing_Sites_20240502.csv"))
  
  testing_map <- testing_sites |> 
    leaflet() |> 
    addProviderTiles("CartoDB.Positron") |> 
    addPolygons(data = sd_zip_shp,
                weight = 0.5,
                color = "#333",
                fillOpacity = 0.3,
                smoothFactor = 0.5,
                highlightOptions = highlightOptions(weight = 2.0,
                                                    color = "#FFF"),
                label = ~ zip) |> 
    addMarkers(data = testing_sites, lng = ~ Longitude, lat = ~ Latitude, popup = ~ Name)
  

  output$mapTestingSites <- renderLeaflet({
    testing_map
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
