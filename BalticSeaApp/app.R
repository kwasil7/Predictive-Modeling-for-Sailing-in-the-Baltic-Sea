#
# This Shiny app demonstrates how to specify three points (Start, Via, End),
# create a rounded route (to 0.5 degrees), and save the route as a tibble.
#

library(shiny)
library(leaflet)
library(geosphere)
library(dplyr)

# Define UI for the application
ui <- fluidPage(
  titlePanel("Sailing Route in the Baltic Sea (Rounded Coordinates)"),
  
  sidebarLayout(
    sidebarPanel(
      # Choose which point to set from map clicks
      radioButtons("point_type", "Point to select:",
                   choices = c("Start", "Via", "End"),
                   inline = TRUE),
      
      # Start point
      numericInput("start_lon", "Start Longitude:", value = 18.0, step = 0.1),
      numericInput("start_lat", "Start Latitude:", value = 55.0, step = 0.1),
      
      # Via point
      numericInput("via_lon", "Via Longitude:", value = 18.5, step = 0.1),
      numericInput("via_lat", "Via Latitude:", value = 55.5, step = 0.1),
      
      # End point
      numericInput("end_lon", "End Longitude:", value = 19.0, step = 0.1),
      numericInput("end_lat", "End Latitude:", value = 56.0, step = 0.1),
      
      actionButton("plot_route", "Plot Route"),
      actionButton("save_route", "Save Route as Dataframe")
    ),
    mainPanel(
      leafletOutput("route_map", height = "600px"),
      tableOutput("route_table"),
      verbatimTextOutput("saved_message")
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  
  # Reactive values to store coordinates and route data
  coords <- reactiveValues(
    start = c(lon = 18.0, lat = 55.0),
    via   = c(lon = 18.5, lat = 55.5),
    end   = c(lon = 19.0, lat = 56.0),
    route_df = NULL,  # Stores the generated route
    saved_route = NULL # Stores the saved route as tibble
  )
  
  # Update coordinates if numeric inputs change
  observeEvent(input$start_lon, { coords$start["lon"] <- input$start_lon })
  observeEvent(input$start_lat, { coords$start["lat"] <- input$start_lat })
  observeEvent(input$via_lon, { coords$via["lon"] <- input$via_lon })
  observeEvent(input$via_lat, { coords$via["lat"] <- input$via_lat })
  observeEvent(input$end_lon, { coords$end["lon"] <- input$end_lon })
  observeEvent(input$end_lat, { coords$end["lat"] <- input$end_lat })
  
  # Update coordinates from map clicks
  observeEvent(input$route_map_click, {
    click <- input$route_map_click
    if (!is.null(click)) {
      if (input$point_type == "Start") {
        coords$start <- c(lon = click$lng, lat = click$lat)
        updateNumericInput(session, "start_lon", value = click$lng)
        updateNumericInput(session, "start_lat", value = click$lat)
      } else if (input$point_type == "Via") {
        coords$via <- c(lon = click$lng, lat = click$lat)
        updateNumericInput(session, "via_lon", value = click$lng)
        updateNumericInput(session, "via_lat", value = click$lat)
      } else {
        coords$end <- c(lon = click$lng, lat = click$lat)
        updateNumericInput(session, "end_lon", value = click$lng)
        updateNumericInput(session, "end_lat", value = click$lat)
      }
    }
  })
  
  # Render an initial leaflet map
  output$route_map <- renderLeaflet({
    leaflet() |>
      addTiles() |>
      setView(
        lng = mean(c(coords$start["lon"], coords$via["lon"], coords$end["lon"])),
        lat = mean(c(coords$start["lat"], coords$via["lat"], coords$end["lat"])),
        zoom = 6
      )
  })
  
  # Plot route and calculate rounded coordinates
  observeEvent(input$plot_route, {
    start <- c(coords$start["lon"], coords$start["lat"])
    via   <- c(coords$via["lon"], coords$via["lat"])
    end   <- c(coords$end["lon"], coords$end["lat"])
    
    # Generate segments: Start->Via and Via->End
    route1 <- gcIntermediate(start, via, n = 50, addStartEnd = TRUE, sp = TRUE)
    route2 <- gcIntermediate(via, end, n = 50, addStartEnd = TRUE, sp = TRUE)
    
    # Convert routes to data frames
    route1_df <- route1 |>
      coordinates() |>
      as.data.frame() |>
      setNames(c("longitude", "latitude"))
    
    route2_df <- route2 |>
      coordinates() |>
      as.data.frame() |>
      setNames(c("longitude", "latitude")) |> 
      slice(-1) # Remove duplicate via point
    
    full_route_df <- bind_rows(route1_df, route2_df) |>
      mutate(
        longitude = round(longitude * 2) / 2, # Round to nearest 0.5
        latitude = round(latitude * 2) / 2
      ) |>
      distinct() # Remove duplicate coordinates after rounding
    
    # Store the rounded route in reactive values
    coords$route_df <- full_route_df
    
    # Render the map with the rounded route and markers
    leafletProxy("route_map") |>
      clearMarkers() |>
      clearShapes() |>
      addPolylines(data = full_route_df, lng = ~longitude, lat = ~latitude, color = "blue") |>
      addCircleMarkers(
        lng = c(start[1], via[1], end[1]),
        lat = c(start[2], via[2], end[2]),
        color = c("purple", "seagreen", "darkblue"),
        label = c("Start", "Via", "End")
      )
  })
  
  # Display the rounded route as a table
  output$route_table <- renderTable({
    coords$route_df
  })
  
  # Save the route as a dataframe/tibble
  observeEvent(input$save_route, {
    if (!is.null(coords$route_df)) {
      coords$saved_route <- coords$route_df |> as_tibble()
      output$saved_message <- renderText("Route has been saved as a dataframe.")
    } else {
      output$saved_message <- renderText("No route to save.")
    }
  })
  
  # Output saved route message
  output$saved_message <- renderText({
    "No route has been saved yet."
  })
}

# Run the application
shinyApp(ui = ui, server = server)
