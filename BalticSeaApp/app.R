#
# This Shiny app allows selecting points interactively on the map (Start, Via, End),
# dynamically updates the route, and supports saving selected points as a tibble.
#

library(shiny)
library(leaflet)
library(geosphere)
library(dplyr)
library(sp)
library(tidyverse)

# Load the dataset during app initialization
baltic_data <- read_rds("baltic_data_for_shiny/baltic_data_model.rds")
baltic_atmospheric_data <- read_rds("baltic_data_for_shiny/baltic_atmospheric_data.rds")

# Glimpse the dataset in the console to confirm it's loaded
print(glimpse(baltic_data))

# Define UI
ui <- navbarPage(
  "Baltic Sea Safety App",
  
  # Tab 1: Route Selection
  tabPanel(
    "Route Selection",
    sidebarLayout(
      sidebarPanel(
        radioButtons("point_type", "Point to select:",
                     choices = c("Start", "Via", "End"),
                     inline = TRUE),
        actionButton("add_point", "Add Point"),
        actionButton("clear_route", "Clear Route"),
        actionButton("plot_route", "Plot Route"),
        actionButton("save_points", "Save Selected Points as Tibble")
      ),
      mainPanel(
        leafletOutput("route_map", height = "600px"),
        tableOutput("route_table"),
        verbatimTextOutput("saved_message"),
        plotOutput("safety_map", height = "800px", width = "1200px"),
        tableOutput("prediction_table")  # Placeholder for the predicted results table
      )
    ),
    selectInput(
      "variable_group", "Select variables to predict:",
      choices = list(
        "Basic variables (safety-related)" = "basic",
        "Extended variables (10 variables)" = "extended",
        "All variables" = "all"
      ),
      selected = "basic"
    ),
     actionButton("start_prediction", "Predict Variables")
  ),
  
  # Tab 2: Plots
  tabPanel(
    "Historical Analysis",
    sidebarLayout(
      sidebarPanel(
        selectInput("plot_var", "Select Variable to Plot:",
                    choices = c("Wind speed" = "wind_speed",
                                "Wave height" = "significant_height_combined_waves_swell",
                                "Mean wave period" = "mean_wave_period",
                                "Sea surface temperature" = "sea_surface_temperature")),
        sliderInput("time_range", "Time Range:",
                    min = as.Date("2012-01-01"),
                    max = as.Date("2013-12-31"),
                    value = c(as.Date("2012-01-01"), as.Date("2013-12-31")),
                    timeFormat = "%Y-%m-%d")
      ),
      mainPanel(
        plotOutput("plot_output"),
        verbatimTextOutput("plot_summary")
      )
    )
  ),
  
  # Tab 3: Atmospheric Data 0.25 Degrees (Refined)
  tabPanel(
    "Atmospheric Data (0.25° Resolution)",
    sidebarLayout(
      sidebarPanel(
        h3("Route Selection"),
        radioButtons(
          "point_type", 
          "Point Type:",
          choices = c("Start", "Via", "End"),
          inline = TRUE
        ),
        actionButton("atmos_add_point", "Add Point", icon = icon("map-marker-alt")),
        actionButton("atmos_clear_route", "Clear Route", icon = icon("trash-alt")),
        actionButton("atmos_plot_route", "Plot Route", icon = icon("route")),
        actionButton("atmos_save_points", "Save Route Points", icon = icon("save")),
        
        hr(),
        
        h3("Prediction Settings"),
        selectInput(
          "atmos_variable_group", 
          "Variables to Predict:",
          choices = list(
            "Basic Variables (Safety-Related)" = "basic",
            "All Atmospheric Variables" = "all_atmos"
          ),
          selected = "basic"
        ),
        dateInput(
          "selected_date", 
          "Select Date for Visualization:",
          value = "2013-01-08",  # Default date
          min = "2013-01-01",
          max = "2013-01-31"
        ),
        actionButton("atmos_predict", "Run Prediction", icon = icon("chart-line")),
        
        hr(),
        
        h3("Messages"),
        verbatimTextOutput("atmos_saved_message")
      ),
      mainPanel(
        h3("Route Map"),
        leafletOutput("atmos_route_map", height = "600px"),
        
        hr(),
        
        h3("Route Points Table"),
        tableOutput("atmos_route_table"),
        
        hr(),
        
        h3("Safety Map"),
        plotOutput("atmos_safety_map", height = "800px", width = "1200px"),
        
        hr(),
        
        h3("Prediction Results"),
        tableOutput("atmos_prediction_table")
      )
    )
  )
)

library(zoo)
library(TTR)
library(paletteer)

# Use in a ggplot2 chart:
scale_colour_paletteer_d("ggsci::light_blue_material")
scale_fill_paletteer_d("ggsci::light_blue_material")

# Define Server Logic
server <- function(input, output, session) {
  
  # Define variable groups
  variable_groups <- list(
    basic = c("wind_speed", "significant_height_combined_waves_swell", "sea_ice_concentration"),
    extended = c("wind_speed", "significant_height_combined_waves_swell", "sea_ice_concentration",
                 "mean_wave_period", "sea_surface_temperature", "mean_sea_level_pressure",
                 "surface_pressure", "max_wind_gust", "instantaneous_wind_gust", "low_cloud_cover"),
    all = colnames(baltic_data)[sapply(baltic_data, is.numeric) & 
                                  !colnames(baltic_data) %in% c("longitude", "latitude")]
  )
  
  # Reactive expression for selected variables
  selected_variables <- reactive({
    variable_groups[[input$variable_group]]
  })
  
  # Display selected variables for debugging
  observe({
    print(selected_variables())
  })
  
  # Define atmospheric variable groups (tab 3)
  atmos_variable_groups <- list(
    basic = c("wind_speed", "sea_ice_concentration"),
    all_atmos = c("wind_speed", "mean_sea_level_pressure", 
                  "sea_surface_temperature", "surface_pressure",
                  "max_wind_gust", "instantaneous_wind_gust",
                  "low_cloud_cover", "total_cloud_cover", 
                  "precipitation_type", 
                  "convective_available_potential_energy",
                  "sea_ice_concentration")
  )
  
  # Reactive expression for selected variables
  atmos_selected_variables <- reactive({
    atmos_variable_groups[[input$atmos_variable_group]]
  })
  
  # Reactive storage for forecast results
  forecast_results <- reactiveVal(NULL)
  
  # Observe "Predict Variables" button click
  observeEvent(input$start_prediction, {
    # Ensure a group is selected
    req(selected_variables())
    req(points$route)  # Ensure route exists
    print("Using selected route for predictions...")
    print("Starting predictions...")  # Debugging message
    
    # Get the selected variables
    selected_vars <- selected_variables()
    
    # Filter data to only include 2012
    training_data <- baltic_data |>
      filter(
        latitude %in% points$route$latitude,
        longitude %in% points$route$longitude,
        year(time) == 2012
      )
    print("Training data filtered.")
    
    # Group by latitude and longitude
    grouped_data <- training_data |>
      group_by(latitude, longitude) |>
      group_split()
    
    # Define forecast parameters
    window_size <- 3
    future_length <- 31 * 3  # 31 days * 3 obs/day
    future_dates <- seq.Date(
      from = as.Date("2013-01-01"),
      to = as.Date("2013-01-31"),
      by = "day"
    )
    future_times <- as.POSIXct(
      paste(
        rep(future_dates, each = 3),
        rep(c("06:00:00", "12:00:00", "18:00:00"), times = length(future_dates))
      ),
      tz = "UTC"
    )
    
    # Initialize list to store results
    results <- vector("list", length(grouped_data))
    
    # Loop through each location
    for (i in seq_along(grouped_data)) { 
      loc_data <- grouped_data[[i]] |>
        arrange(time)
      
      # Store forecast results for each selected variable
      forecast_list <- vector("list", length(selected_vars))
      
      for (v in seq_along(selected_vars)) {
        var_name <- selected_vars[v]
        
        # Extract and interpolate time series for the current variable
        ts_data <- loc_data |>
          pull(var_name)
        
        # Interpolate missing values
        ts_data <- na.approx(ts_data, rule = 2)
        
        # Generate forecasts using WMA
        wma_forecasts <- numeric(future_length)
        for (j in seq_along(wma_forecasts)) {
          recent_values <- c(ts_data, wma_forecasts[1:(j - 1)])
          recent_values <- na.approx(recent_values, rule = 2)
          
          if (length(recent_values) < window_size) {
            wma_forecasts[j] <- NA
          } else {
            wma_result <- WMA(recent_values, n = window_size)
            wma_forecasts[j] <- tail(wma_result, 1)
          }
        }
        
        forecast_list[[v]] <- tibble(!!paste0("predicted_", var_name) := wma_forecasts)
      }
      
      # Combine all predictions for this location into one tibble
      loc_results <- bind_cols(
        tibble(
          latitude = unique(loc_data$latitude),
          longitude = unique(loc_data$longitude),
          time = future_times
        ),
        bind_cols(forecast_list)
      )
      
      results[[i]] <- loc_results
    }
    
    # Combine all results into a single data frame and update reactiveVal
    forecast_results(bind_rows(results))
    print("Predictions complete.")
    print("Updated forecast_results:")
    print(head(forecast_results()))
    
    forecast_results <- forecast_results() |> 
      mutate(
        safety_label = case_when(
          predicted_wind_speed >= 10.8 ~ "Unsafe",
          predicted_significant_height_combined_waves_swell >= 3 ~ "Unsafe",
          predicted_sea_ice_concentration > 0.3 ~ "Risky",
          predicted_wind_speed >= 8.0 & predicted_wind_speed < 10.8 ~ "Risky",
          TRUE ~ "Safe"
        )
      )
    forecast_results(forecast_results)
    print("Safety labels assigned.")
    print(head(forecast_results()))
    
  })
  
  # Use the forecasted results in your app outputs
  output$prediction_table <- renderTable({
    print("Rendering forecast table...")  # Debugging message
    forecast_results()
  })
  
  output$safety_map <- renderPlot({
    req(forecast_results())  # Ensure predictions are complete
    
    # Merge full Baltic Sea data with predictions
    safety_map_data <- baltic_data |> 
      select(longitude, latitude) |>  # Keep full Baltic Sea grid
      distinct() |>  # Remove duplicate coordinates
      left_join(
        forecast_results() |> select(latitude, longitude, safety_label),
        by = c("latitude", "longitude")
      ) |> 
      mutate(
        safety_label = factor(
          safety_label,
          levels = c("Safe", "Risky", "Unsafe")
        )
      )
    
    # Create the hexbin map
    ggplot(safety_map_data, aes(x = longitude, y = latitude)) +
      geom_hex(
        aes(fill = safety_label), bins = 22
      ) +
      scale_fill_paletteer_d(
        "ggsci::light_blue_material",
        name = "Safety Label",
        na.value = "gray"  # Gray for undefined regions
      ) +
      coord_quickmap() +  # Keep real-world aspect ratio
      expand_limits(
        x = range(baltic_data$longitude, na.rm = TRUE),
        y = range(baltic_data$latitude, na.rm = TRUE)
      ) +
      labs(
        x = "Longitude",
        y = "Latitude",
        title = "Safety Labels Across the Baltic Sea"
      ) +
      theme_minimal() +
      theme(
        legend.title = element_text(size = 14),  # Larger legend title
        legend.text = element_text(size = 12),   # Larger legend text
        legend.key.size = unit(1.2, "cm")        # Larger legend keys
      )
  })
  
  # Reactive storage for points
  points <- reactiveValues(
    data = tibble(type = character(), longitude = numeric(), latitude = numeric()),
    saved_points = NULL # Tibble for saved points
  )
  
  # Observe map clicks and add points dynamically
  observeEvent(input$route_map_click, {
    click <- input$route_map_click
    if (!is.null(click)) {
      new_point <- tibble(
        type = input$point_type,
        longitude = click$lng,
        latitude = click$lat
      )
      points$data <- bind_rows(points$data, new_point)
      
      # Update map with markers for the selected point
      leafletProxy("route_map") |>
        addCircleMarkers(
          lng = click$lng,
          lat = click$lat,
          color = ifelse(input$point_type == "Start", "green",
                         ifelse(input$point_type == "End", "red", "orange")),
          label = paste(input$point_type, "(", round(click$lng, 2), ",", round(click$lat, 2), ")")
        )
    }
  })
  
  # Clear all points
  observeEvent(input$clear_route, {
    points$data <- tibble(type = character(), longitude = numeric(), latitude = numeric())
    leafletProxy("route_map") |> clearMarkers() |> clearShapes()
    output$saved_message <- renderText("Route cleared.")
  })
  
  # Plot the route
  observeEvent(input$plot_route, {
    if (nrow(points$data) < 2) {
      output$saved_message <- renderText("At least a Start and End point are required.")
      return(NULL)
    }
    
    # Sort points: Start -> Via -> End
    sorted_points <- points$data |>
      arrange(match(type, c("Start", "Via", "End")))
    
    # Generate the route using gcIntermediate for each segment
    route <- bind_rows(lapply(1:(nrow(sorted_points) - 1), function(i) {
      gcIntermediate(
        c(sorted_points$longitude[i], sorted_points$latitude[i]),
        c(sorted_points$longitude[i + 1], sorted_points$latitude[i + 1]),
        n = 50,
        addStartEnd = TRUE,
        sp = TRUE
      ) |>
        coordinates() |>
        as.data.frame() |>
        setNames(c("longitude", "latitude"))
    })) |>
      mutate(
        longitude = round(longitude * 2) / 2, # Round to nearest 0.5
        latitude = round(latitude * 2) / 2
      ) |>
      distinct()
    
    # Store the route and update the map
    points$route <- route
    
    leafletProxy("route_map") |>
      clearShapes() |>
      addPolylines(data = route, lng = ~longitude, lat = ~latitude, color = "blue")
    
    output$saved_message <- renderText("Route plotted.")
  })
  
  # Save selected points as a tibble
  observeEvent(input$save_points, {
    if (nrow(points$data) > 0) {
      points$saved_points <- points$data |> as_tibble()
      output$saved_message <- renderText("Selected points have been saved as a tibble.")
    } else {
      output$saved_message <- renderText("No points to save.")
    }
  })
  
  # Display the route as a table
  output$route_table <- renderTable({
    points$route
  })
  
  # Initial map rendering
  output$route_map <- renderLeaflet({
    leaflet() |> addTiles() |> setView(lng = 18.5, lat = 55.5, zoom = 6)
  })
  
  #Plot logic
  # Reactive expression to filter the data based on the selected time range
  filtered_data <- reactive({
    baltic_data |>
      filter(as.Date(time) >= input$time_range[1] & as.Date(time) <= input$time_range[2])
  })
  
  output$plot_output <- renderPlot({
    data <- filtered_data()
    
    ggplot(data, aes(x = as.Date(time), y = .data[[input$plot_var]])) +
      geom_line(color = "blue") +
      theme_minimal() +
      labs(
        title = paste("Trend of", input$plot_var, "over time"),
        x = "Date",
        y = gsub("_", " ", input$plot_var, fixed = TRUE)
      ) +
      theme(
        plot.title = element_text(size = 16, face = "bold"),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12)
      )
  })
  
  # Atmospheric tab map
  # Define a new reactiveValues for atmospheric points
  atmos_points <- reactiveValues(
    data = tibble(type = character(), longitude = numeric(), latitude = numeric()),
    route = NULL
  )
  
  # Observe map clicks and add points dynamically for atmospheric tab
  observeEvent(input$atmos_route_map_click, {
    click <- input$atmos_route_map_click
    if (!is.null(click)) {
      new_point <- tibble(
        type = input$point_type,
        longitude = click$lng,
        latitude = click$lat
      )
      atmos_points$data <- bind_rows(atmos_points$data, new_point)
      
      # Update map with markers for the selected point
      leafletProxy("atmos_route_map") |>
        addCircleMarkers(
          lng = click$lng,
          lat = click$lat,
          color = ifelse(input$point_type == "Start", "green",
                         ifelse(input$point_type == "End", "red", "orange")),
          label = paste(input$point_type, "(", round(click$lng, 2), ",", round(click$lat, 2), ")")
        )
    }
  })
  
  # Clear all points for atmospheric tab
  observeEvent(input$atmos_clear_route, {
    atmos_points$data <- tibble(type = character(), longitude = numeric(), latitude = numeric())
    leafletProxy("atmos_route_map") |> clearMarkers() |> clearShapes()
    output$atmos_saved_message <- renderText("Route cleared.")
  })
  
  # Plot the route for atmospheric tab
  observeEvent(input$atmos_plot_route, {
    if (nrow(atmos_points$data) < 2) {
      output$atmos_saved_message <- renderText("At least a Start and End point are required.")
      return(NULL)
    }
    
    # Sort points: Start -> Via -> End
    sorted_points <- atmos_points$data |>
      arrange(match(type, c("Start", "Via", "End")))
    
    # Generate the route
    route <- bind_rows(lapply(1:(nrow(sorted_points) - 1), function(i) {
      gcIntermediate(
        c(sorted_points$longitude[i], sorted_points$latitude[i]),
        c(sorted_points$longitude[i + 1], sorted_points$latitude[i + 1]),
        n = 20,
        addStartEnd = TRUE,
        sp = TRUE
      ) |>
        coordinates() |>
        as.data.frame() |>
        setNames(c("longitude", "latitude"))
    })) |>
      mutate(
        longitude = round(longitude * 4) / 4, # Round to nearest 0.25
        latitude = round(latitude * 4) / 4
      ) |>
      distinct()
    
    # Store the route and update the map
    atmos_points$route <- route
    
    leafletProxy("atmos_route_map") |>
      clearShapes() |>
      addPolylines(data = route, lng = ~longitude, lat = ~latitude, color = "blue")
    
    output$atmos_saved_message <- renderText("Route plotted.")
  })
  
  # Save selected points as a tibble for atmospheric tab
  observeEvent(input$atmos_save_points, {
    if (nrow(atmos_points$data) > 0) {
      atmos_points$saved_points <- atmos_points$data |> as_tibble()
      output$atmos_saved_message <- renderText("Selected points have been saved as a tibble.")
    } else {
      output$atmos_saved_message <- renderText("No points to save.")
    }
  })
  
  # Display the route as a table in atmospheric tab
  output$atmos_route_table <- renderTable({
    atmos_points$route
  })
  
  # Initial map rendering for atmospheric tab
  output$atmos_route_map <- renderLeaflet({
    leaflet() |> addTiles() |> setView(lng = 18.5, lat = 55.5, zoom = 6)
  })
  
  # Prediction logic for atmospheric variables
  # Reactive storage for atmospheric forecast results
  atmos_forecast_results <- reactiveVal(NULL)
  
  # Observe "Predict Variables" button click for atmospheric tab
  observeEvent(input$atmos_predict, {
    # Ensure a group is selected
    req(atmos_selected_variables())
    req(atmos_points$route)  # Ensure route exists
    print("Using selected route for atmospheric predictions...")
    print("Starting atmospheric predictions...")  # Debugging message
    
    # Get the selected variables
    selected_vars <- atmos_selected_variables()
    
    # Filter data to only include 2012 and route coordinates
    training_data <- baltic_atmospheric_data |>
      filter(
        latitude %in% atmos_points$route$latitude,
        longitude %in% atmos_points$route$longitude,
        year(time) == 2012
      )
    print("Atmospheric training data filtered.")
    
    # Group by latitude and longitude
    grouped_data <- training_data |>
      group_by(latitude, longitude) |>
      group_split()
    
    # Define forecast parameters
    window_size <- 3
    future_length <- 31 * 3  # 31 days * 3 obs/day
    future_dates <- seq.Date(
      from = as.Date("2013-01-01"),
      to = as.Date("2013-01-31"),
      by = "day"
    )
    future_times <- as.POSIXct(
      paste(
        rep(future_dates, each = 3),
        rep(c("06:00:00", "12:00:00", "18:00:00"), times = length(future_dates))
      ),
      tz = "UTC"
    )
    
    # Initialize list to store results
    results <- vector("list", length(grouped_data))
    
    # Loop through each location
    for (i in seq_along(grouped_data)) { 
      loc_data <- grouped_data[[i]] |>
        arrange(time)
      
      # Store forecast results for each selected variable
      forecast_list <- vector("list", length(selected_vars))
      
      for (v in seq_along(selected_vars)) {
        var_name <- selected_vars[v]
        
        # Extract and interpolate time series for the current variable
        ts_data <- loc_data |>
          pull(var_name)
        
        # Interpolate missing values
        ts_data <- na.approx(ts_data, rule = 2)
        
        # Generate forecasts using WMA
        wma_forecasts <- numeric(future_length)
        for (j in seq_along(wma_forecasts)) {
          recent_values <- c(ts_data, wma_forecasts[1:(j - 1)])
          recent_values <- na.approx(recent_values, rule = 2)
          
          if (length(recent_values) < window_size) {
            wma_forecasts[j] <- NA
          } else {
            wma_result <- WMA(recent_values, n = window_size)
            wma_forecasts[j] <- tail(wma_result, 1)
          }
        }
        
        forecast_list[[v]] <- tibble(!!paste0("predicted_", var_name) := wma_forecasts)
      }
      
      # Combine all predictions for this location into one tibble
      loc_results <- bind_cols(
        tibble(
          latitude = unique(loc_data$latitude),
          longitude = unique(loc_data$longitude),
          time = future_times
        ),
        bind_cols(forecast_list)
      )
      
      results[[i]] <- loc_results
    }
    
    # Combine all results into a single data frame and update reactiveVal
    atmos_forecast_results(bind_rows(results))
    print("Atmospheric predictions complete.")
    print("Updated atmos_forecast_results:")
    print(head(atmos_forecast_results()))
  })
  
  # Add safety labels to atmospheric forecast results
  observeEvent(input$atmos_predict, {
    # Ensure atmospheric forecast results exist
    req(atmos_forecast_results())
    
    # Add safety labels to the forecast results
    updated_results <- atmos_forecast_results() |> 
      mutate(
        safety_label = case_when(
          predicted_wind_speed >= 10.8 ~ "Unsafe",
          predicted_sea_ice_concentration > 0.3 ~ "Risky",
          predicted_wind_speed >= 8.0 & predicted_wind_speed < 10.8 ~ "Risky",
          TRUE ~ "Safe"
        )
      )
    
    # Update the reactiveVal with safety labels
    atmos_forecast_results(updated_results)
    print("Safety labels assigned for atmospheric data.")
    print(head(atmos_forecast_results()))
  })
  
  # Atmospheric safety map with refined hex bins
  output$atmos_safety_map <- renderPlot({
    req(atmos_forecast_results())  # Ensure predictions are complete
    req(input$selected_date)  # Ensure a date is selected
    
    # Get the world map and filter for the Baltic Sea region
    baltic_map <- map_data("world") |>
      filter(lat > 53, lat < 66, long > 9, long < 31)
    
    # Filter safety data for the selected date and join with route points
    safety_map_data <- atmos_forecast_results() |> 
      filter(as.Date(time) == as.Date(input$selected_date)) |>  # Filter by selected date
      semi_join(atmos_points$route, by = c("longitude", "latitude")) |>  # Only use route points
      mutate(
        safety_label = factor(
          safety_label,
          levels = c("Safe", "Risky", "Unsafe")
        )
      )
    
    # Create the map with safety labels
    ggplot() +
      # Add Baltic Sea map as background
      geom_polygon(data = baltic_map, aes(x = long, y = lat, group = group),
                   fill = "gray80", color = "black") +
      # Add hexbin safety data
      geom_hex(
        data = safety_map_data,
        aes(x = longitude, y = latitude, fill = safety_label),
        bins = 40  # Adjust bin size for finer granularity
      ) +
      scale_fill_paletteer_d(
        "ggsci::light_blue_material",
        name = "Safety Label",
        na.value = "gray"  # Gray for undefined regions
      ) +
      coord_quickmap() +  # Keep real-world aspect ratio
      labs(
        x = "Longitude",
        y = "Latitude",
        title = paste("Safety Labels Along the Route (", input$selected_date, ")", sep = "")
      ) +
      theme_minimal() +
      theme(
        legend.title = element_text(size = 14),  # Larger legend title
        legend.text = element_text(size = 12),   # Larger legend text
        legend.key.size = unit(1.2, "cm")        # Larger legend keys
      )
  })
  
  
  # Render atmospheric prediction table
  output$atmos_prediction_table <- renderTable({
    print("Rendering atmospheric forecast table...")  # Debugging message
    atmos_forecast_results()
  })
  
}

# Run the App
shinyApp(ui = ui, server = server)
