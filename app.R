#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(here)
library(dplyr)
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
            choices = setNames(services_standard$ID, services_standard$Name)),
    HTML("<br/><b><u>Regular hours</u></b>"),
    checkboxGroupInput("regDays",
                       HTML("<b>Days open:</b>"),
                       choices = list("Monday" = "Mon",
                                      "Tuesday" = "Tue", 
                                      "Wednesday" = "Wed", 
                                      "Thursday" = "Thu", 
                                      "Friday" = "Fri")),
    # radioButtons("regHoursType", HTML("<b>Hours filter type:</b>"),
    #              choices = list("Open any time during hours selected below" = "Inclusive",
    #                             "Open hours must fall within hours selected below" = "Exclusive"),
    #              selected = "Inclusive"),
    # sliderInput("regOpen", HTML("<b>Opens after:</b>"),
    #           min = as.POSIXct("08:00", format="%H:%M", tz = "America/Los_Angeles"),
    #           max = as.POSIXct("14:30", format="%H:%M", tz = "America/Los_Angeles"),
    #           value = as.POSIXct("08:00", format="%H:%M", tz = "America/Los_Angeles"),
    #           timeFormat = "%H:%M",
    #           step = 1800, # 1800 seconds = 30 minutes
    #           ticks = FALSE), # Set to TRUE if you want to display ticks              
    # sliderInput("regClose", HTML("<b>Closes by:</b>"),
    #           min = as.POSIXct("16:00", format="%H:%M", tz = "America/Los_Angeles"),
    #           max = as.POSIXct("18:30", format="%H:%M", tz = "America/Los_Angeles"),
    #           value = as.POSIXct("18:30", format="%H:%M", tz = "America/Los_Angeles"),
    #           timeFormat = "%H:%M",
    #           step = 1800, # 1800 seconds = 30 minutes
    #           ticks = FALSE), # Set to TRUE if you want to display ticks
  sliderInput("regHours", HTML("<b>Times open:</b>"),
            min = as.POSIXct("08:00", format="%H:%M", tz = "America/Los_Angeles"),
            max = as.POSIXct("18:30", format="%H:%M", tz = "America/Los_Angeles"),
            value = c(as.POSIXct("08:00", format="%H:%M", tz = "America/Los_Angeles"), 
                      as.POSIXct("18:30", format="%H:%M", tz = "America/Los_Angeles")),
            timeFormat = "%H:%M",
            step = 1800, # 1800 seconds = 30 minutes
            ticks = FALSE,
            animate = TRUE), # Set to TRUE if you want to display ticks
  textOutput("selectedTimeRange")
  ),
  leafletOutput("mapTestingSites")
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {  
  # Get services site data
  service_sites <- read.csv(here("data", "SanDiego_Demo_Locations.csv"))  
  
  output$mapTestingSites <- renderLeaflet({
    if (is.null(input$services)) {
      filtered_sites <- service_sites
    } else {
      filtered_sites <- service_sites[service_sites$StandardServices %in% input$services, ]
    }
    
    # Whether site is mobile site
    if (input$mobile != "All") {
      filtered_sites <- filtered_sites[filtered_sites$Mobile == input$mobile, ]
    }
    
    # Days site is open
    if (!is.null(input$regDays)) {
      filtered_sites <- filtered_sites[filtered_sites$RegDay %in% input$regDays, ]
    }

    # Hours site is open
    # Time filtering based on slider input    
    start_time <- format(input$regHours[1], "%H:%M", tz = "America/Los_Angeles")
    end_time <- format(input$regHours[2], "%H:%M", tz = "America/Los_Angeles")
    
    # For testing
    # output$selectedTimeRange <- renderText({
    #   paste("Selected Time Range: ", start_time, " - ", end_time)
    # })
    
    filtered_sites <- filtered_sites[filtered_sites$RegStart <= end_time & filtered_sites$RegEnd >= start_time, ]    

    filtered_sites <- filtered_sites %>%
      group_by(ID) %>%
      arrange(ID) %>%
      slice(1) %>%
      ungroup()
    
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
