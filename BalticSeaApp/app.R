#
# This Shiny app allows selecting points interactively on the map (Start, Via, End),
# dynamically updates the route, and supports saving selected points as a tibble.
#

library(shiny)
library(leaflet)
library(geosphere)
library(sp)
library(tidyverse)
library(shinyjs)
library(DT)

# Load the dataset during app initialization
baltic_data <- read_rds("baltic_data_for_shiny/baltic_data_model.rds")
baltic_atmospheric_data <- read_rds("baltic_data_for_shiny/baltic_atmospheric_data.rds")

# Glimpse the dataset in the console to confirm it's loaded
print(glimpse(baltic_data))

# Define UI
ui <- navbarPage(
  title = "Baltic Sea Safety App",
  
  # Tab 1: Atmospheric Data (0.25° Resolution)
  tabPanel(
    "Atmospheric Data (0.25° Resolution)",
    sidebarLayout(
      sidebarPanel(
        useShinyjs(),
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
        dateRangeInput(
          "prediction_date_range", 
          "Select Prediction Date Range:",
          start = "2025-01-01", 
          end = "2025-01-31",
          min = "2000-01-01",
          max = "2100-12-31"
        ),
        actionButton("atmos_predict", "Run Prediction", icon = icon("chart-line")),
        actionButton("atmos_clear_all", "Clear Everything", icon = icon("broom")),
        
        # Info text about saving the route
        div(
          style = "margin-top: 15px; color: red; font-weight: bold;",
          "Note: Please save the route first before running predictions."
        ),
        
        hr(),
        
        h3("Visualization Settings"),
        dateInput(
          "selected_date", 
          "Select Date for Visualization:",
          value = Sys.Date()
        ),
        selectInput(
          "selected_time", 
          "Select Time for Visualization (Optional):",
          choices = c("All Day" = "all_day", "06:00 UTC" = "06:00:00", 
                      "12:00 UTC" = "12:00:00", "18:00 UTC" = "18:00:00"),
          selected = "all_day"
        ),
        actionButton("change_visualization", "Visualize", icon = icon("sync")),
        
        hr(),
        
        h3("Messages"),
        verbatimTextOutput("atmos_saved_message")
      ),
      mainPanel(
        h3("Route Map"),
        leafletOutput("atmos_route_map", height = "600px"),
        
        hr(),
        
        h3("Route Points Table"),
        DTOutput("atmos_route_table"),
        
        hr(),
        
        h3("Safety Map"),
        plotOutput("atmos_safety_map", height = "800px", width = "1200px"),
        
        hr(),
        
        h3("Interactive Map"),
        girafeOutput("atmos_interactive_map", height = "800px", width = "1200px"),
        
        hr(),
        
        h3("Prediction Results"),
        DTOutput("atmos_prediction_table"),
        
        hr(),
        
        div(id = "additional_plots", style = "display: none;",
            h3("Atmospheric Variables Insights"),
            plotOutput("wind_speed_plot", height = "400px"),
            plotOutput("sea_level_pressure_plot", height = "400px"),
            plotOutput("sea_surface_temperature_plot", height = "400px")
        )
      )
    )
  ),
  
  # Tab 2: Route Selection (0.5° Resolution)
  tabPanel(
    "Route Selection (0.5° Resolution)",
    sidebarLayout(
      sidebarPanel(
        useShinyjs(),
        h3("Route Selection"),
        radioButtons(
          "route_point_type",  # Unique ID
          "Point Type:",
          choices = c("Start", "Via", "End"),
          inline = TRUE
        ),
        actionButton("route_add_point", "Add Point", icon = icon("map-marker-alt")),
        actionButton("route_clear_route", "Clear Route", icon = icon("trash-alt")),
        actionButton("route_plot_route", "Plot Route", icon = icon("route")),
        actionButton("route_save_points", "Save Route Points", icon = icon("save")),
        
        hr(),
        
        h3("Prediction Settings"),
        selectInput(
          "route_variable_group",  # Unique ID
          "Variables to Predict:",
          choices = list(
            "Basic Variables (Safety-Related)" = "basic",
            "Extended Variables (10 Variables)" = "extended",
            "All Variables" = "all"
          ),
          selected = "basic"
        ),
        dateRangeInput(
          "route_prediction_date_range",  # Unique ID
          "Select Prediction Date Range:",
          start = "2025-01-01", 
          end = "2025-01-31",
          min = "2000-01-01",
          max = "2100-12-31"
        ),
        actionButton("route_start_prediction", "Run Prediction", icon = icon("chart-line")),
        actionButton("route_clear_all", "Clear Everything", icon = icon("broom")),
        
        # Info text about saving the route
        div(
          id = "route_prediction_warning",
          style = "color: red; font-weight: bold;",
          "Please save the route before running predictions."
        ),
        
        hr(),
        
        h3("Visualization Settings"),
        dateInput(
          "route_selected_date",  # Unique ID
          "Select Date for Visualization:",
          value = Sys.Date()
        ),
        selectInput(
          "route_selected_time",  # Unique ID
          "Select Time for Visualization (Optional):",
          choices = c("All Day" = "all_day", "06:00 UTC" = "06:00:00", 
                      "12:00 UTC" = "12:00:00", "18:00 UTC" = "18:00:00"),
          selected = "all_day"
        ),
        actionButton("route_change_visualization", "Visualize", icon = icon("sync")),
        
        hr(),
        
        h3("Messages"),
        verbatimTextOutput("route_saved_message")
      ),
      mainPanel(
        h3("Route Map"),
        leafletOutput("route_map", height = "600px"),
        
        hr(),
        
        h3("Route Points Table"),
        DTOutput("route_table"),
        
        hr(),
        
        h3("Safety Map"),
        plotOutput("safety_map", height = "800px", width = "1200px"),
        
        hr(),
        
        h3("Prediction Results"),
        DTOutput("route_prediction_table")
      )
    )
  ),
  
  # Tab 3: Historical Analysis
  tabPanel(
    "Historical Analysis",
    sidebarLayout(
      sidebarPanel(
        useShinyjs(),
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
  )
)

library(zoo)
library(TTR)
library(paletteer)
library(forecast)
library(ggiraph)

# Use in a ggplot2 chart:
scale_colour_paletteer_d("ggsci::light_blue_material")
scale_fill_paletteer_d("ggsci::light_blue_material")

# Define Server Logic
server <- function(input, output, session) {
  

# Atmospheric tab ---------------------------------------------------------
  
  # Define atmospheric variable groups
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
  
  # Reactive expression for selected atmospheric variables
  atmos_selected_variables <- reactive({
    atmos_variable_groups[[input$atmos_variable_group]]
  })
  
  # Reactive storage for atmospheric points
  atmos_points <- reactiveValues(
    data = tibble(type = character(), longitude = numeric(), latitude = numeric()),
    route = NULL,
    route_saved = FALSE
  )
  
  # Observe map clicks for atmospheric route selection
  observeEvent(input$atmos_route_map_click, {
    click <- input$atmos_route_map_click
    if (!is.null(click)) {
      new_point <- tibble(
        type = input$point_type,
        longitude = click$lng,
        latitude = click$lat
      )
      atmos_points$data <- bind_rows(atmos_points$data, new_point)
      
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
  
  # Plot the atmospheric route
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
    
    atmos_points$route <- route
    
    leafletProxy("atmos_route_map") |>
      clearShapes() |>
      addPolylines(data = route, lng = ~longitude, lat = ~latitude, color = "blue")
    
    output$atmos_saved_message <- renderText("Route plotted.")
  })
  
  # Save selected points for atmospheric tab
  observeEvent(input$atmos_save_points, {
    if (nrow(atmos_points$data) > 0) {
      atmos_points$saved_points <- atmos_points$data |> as_tibble()
      atmos_points$route_saved <- TRUE
      output$atmos_saved_message <- renderText("Selected points have been saved as a tibble.")
    } else {
      output$atmos_saved_message <- renderText("No points to save.")
    }
  })
  
  # Display atmospheric route table
  output$atmos_route_table <- renderDT({
    req(atmos_points$route)
    datatable(
      atmos_points$route,
      options = list(
        pageLength = 5,
        dom = 'tp',
        scrollX = TRUE
      )
    )
  })
  
  # Atmospheric route map initialization
  output$atmos_route_map <- renderLeaflet({
    leaflet() |> addTiles() |> setView(lng = 18.5, lat = 55.5, zoom = 6)
  })
  
  # Reactive storage for atmospheric forecast results
  atmos_forecast_results <- reactiveVal(NULL)
  
  # Observe "Predict Variables" button for atmospheric tab
  observeEvent(input$atmos_predict, {
    if (as.Date(input$prediction_date_range[1]) > as.Date(input$prediction_date_range[2])) {
      showNotification("Start date cannot be later than end date!", type = "error")
      return()
    }
    req(atmos_selected_variables())
    req(atmos_points$route)
    req(atmos_points$route_saved)
    
    print("Using selected route for atmospheric predictions...")
    print("Starting atmospheric predictions...")
    
    # Forecast logic for atmospheric data...
    # Get the selected variables
    selected_vars <- atmos_selected_variables()
    
    # Get the prediction date range
    prediction_dates <- seq.Date(
      from = as.Date(input$prediction_date_range[1]),
      to = as.Date(input$prediction_date_range[2]),
      by = "day"
    )
    
    future_times <- as.POSIXct(
      paste(
        rep(prediction_dates, each = 3),
        rep(c("06:00:00", "12:00:00", "18:00:00"), times = length(prediction_dates))
      ),
      tz = "UTC"
    )
    
    # Filter data
    training_data <- baltic_atmospheric_data |>
      filter(
        latitude %in% atmos_points$route$latitude,
        longitude %in% atmos_points$route$longitude,
        year(time) >= 2012 & year(time) < 2014
      )
    print("Atmospheric training data filtered.")
    
    # Group by latitude and longitude
    grouped_data <- training_data |>
      group_by(latitude, longitude) |>
      group_split()
    
    # Define forecast parameters
    window_size <- 3  # For SMA, WMA, and EMA
    future_length <- length(future_times)
    
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
          pull(var_name) |> 
          na.approx(rule = 2)  # Interpolate missing values
        
        # Initialize the forecast array
        forecasts <- numeric(future_length)
        
        # Handle predictions using predicted values
        for (j in seq_along(forecasts)) {
          recent_values <- c(ts_data, forecasts[1:(j - 1)])
          recent_values <- na.approx(recent_values, rule = 2)
          
          if (length(recent_values) < window_size) {
            forecasts[j] <- NA
          } else if (var_name %in% c("mean_sea_level_pressure", "sea_surface_temperature")) {
            # Use EMA for variables with gradual trends
            ema_result <- EMA(recent_values, n = 36, alpha = 0.2)  # Adjust n and alpha for week-to-week variability
            forecasts[j] <- tail(ema_result, 1)
          } else if (var_name %in% c("low_cloud_cover", "total_cloud_cover", "precipitation_type")) {
            # Use SMA for stable variables
            sma_result <- SMA(recent_values, n = 9)  # 3-day smoothing to preserve variability
            forecasts[j] <- tail(sma_result, 1)
          } else {
            # Use EMA for other variables with moderate variability
            ema_result <- EMA(recent_values, n = 3, alpha = 0.4)  # Shorter-term smoothing
            forecasts[j] <- tail(ema_result, 1)
          }
        }
        
        # Store the forecasts in the forecast list
        forecast_list[[v]] <- tibble(!!paste0("predicted_", var_name) := forecasts)
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
  
  # Enable/disable prediction button based on route_saved
  observe({
    if (isTRUE(atmos_points$route_saved)) {
      shinyjs::enable("atmos_predict")
    } else {
      shinyjs::disable("atmos_predict")
    }
  })
  
  # Observe "Change Visualization" button click
  observeEvent(input$change_visualization, {
    output$atmos_safety_map <- renderPlot({
      req(atmos_forecast_results())
      req(input$selected_date)
      
      # code for rendering atmos_safety_map
      # Get the world map and filter for the Baltic Sea region
      baltic_map <- map_data("world") |>
        filter(lat > 53, lat < 66, long > 9, long < 31)
      
      # Filter safety data for the selected date
      safety_map_data <- atmos_forecast_results() |> 
        filter(as.Date(time) == as.Date(input$selected_date))  # Filter by selected date
      
      # Apply time filtering if a specific time is selected
      if (input$selected_time != "all_day") {
        safety_map_data <- safety_map_data |>
          filter(format(time, "%H:%M:%S") == input$selected_time)
      }
      
      # Join with route points
      safety_map_data <- safety_map_data |> 
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
          title = paste("Safety Labels Along the Route (", 
                        input$selected_date, 
                        ifelse(input$selected_time != "all_day", paste("at", input$selected_time), ""), 
                        ")", sep = "")
        ) +
        theme_minimal() +
        theme(
          legend.title = element_text(size = 14),  # Larger legend title
          legend.text = element_text(size = 12),   # Larger legend text
          legend.key.size = unit(1.2, "cm")        # Larger legend keys
        )
    })
  })
  
  # Render atmospheric prediction table
  output$atmos_prediction_table <- renderDT({
    req(atmos_forecast_results())
    # Convert the time column to a readable format
    predictions <- atmos_forecast_results() |> 
      mutate(time = as.POSIXct(time, origin = "1970-01-01", tz = "UTC"))  # Convert UNIX timestamp
    
    datatable(
      predictions,
      options = list(
        pageLength = 5,  # Show only 5 rows initially
        dom = 'tp',  # Hide the search bar and ordering
        scrollX = TRUE  # Enable horizontal scrolling
      )
    )
  })
  
  # Show/Hide Additional Plots Based on Prediction Variables
  observe({
    if (input$atmos_variable_group == "all_atmos") {
      shinyjs::show("additional_plots")
    } else {
      shinyjs::hide("additional_plots")
    }
  })
  
  # Wind Speed Plot
  output$wind_speed_plot <- renderPlot({
    req(atmos_forecast_results())
    
    atmos_forecast_results() |>
      ggplot(aes(x = time, y = predicted_wind_speed)) +
      geom_line(color = "blue") +
      geom_smooth(se = FALSE, color = "red") +
      labs(
        x = "Time", 
        y = "Wind Speed [m/s]",
        title = "Predicted Wind Speed Over Time"
      ) +
      theme_minimal()
  })
  
  # Mean Sea Level Pressure Plot
  output$sea_level_pressure_plot <- renderPlot({
    req(atmos_forecast_results())
    
    atmos_forecast_results() |>
      mutate(predicted_mean_sea_level_pressure_hpa = predicted_mean_sea_level_pressure / 100) |>
      ggplot(aes(x = time, y = predicted_mean_sea_level_pressure_hpa)) +
      geom_line(color = "darkgreen") +
      geom_smooth(se = FALSE, color = "orange") +
      labs(
        x = "Time",
        y = "Sea Level Pressure [hPa]",
        title = "Predicted Mean Sea Level Pressure Over Time"
      ) +
      theme_minimal()
  })
  
  # Sea Surface Temperature Plot
  output$sea_surface_temperature_plot <- renderPlot({
    req(atmos_forecast_results())
    
    atmos_forecast_results() |>
      mutate(predicted_sea_surface_temperature_c = predicted_sea_surface_temperature - 273.15) |>
      ggplot(aes(x = time, y = predicted_sea_surface_temperature_c)) +
      geom_line(color = "blue") +
      geom_smooth(se = FALSE, color = "aquamarine") +
      labs(
        x = "Time",
        y = "Sea Surface Temperature [°C]",
        title = "Predicted Sea Surface Temperature Over Time"
      ) +
      theme_minimal()
  })
  
  # Clear all for atmospheric tab
  observeEvent(input$atmos_clear_all, {
    # Clear route data
    atmos_points$data <- tibble(type = character(), longitude = numeric(), latitude = numeric())
    atmos_points$route <- NULL
    atmos_points$route_saved <- FALSE
    leafletProxy("atmos_route_map") |> clearMarkers() |> clearShapes()
    
    # Clear prediction results
    atmos_forecast_results(NULL)
    
    # Reset UI inputs
    updateDateRangeInput(session, "prediction_date_range", 
                         start = "2025-01-01", end = "2025-01-31")
    updateDateInput(session, "selected_date", value = Sys.Date())
    updateSelectInput(session, "selected_time", selected = "all_day")
    shinyjs::reset("atmos_variable_group")
    
    showNotification("All inputs and predictions cleared.", type = "message")
  })
  

# Atmospheric interactive map ---------------------------------------------

  # Observe "Create Interactive Plot" button click
  observeEvent(input$change_visualization, {
    output$atmos_interactive_map <- renderGirafe({
      req(atmos_forecast_results())
      req(input$selected_date)
      
      # Get the world map and filter for the Baltic Sea region
      baltic_map <- map_data("world") |>
        filter(lat > 53, lat < 66, long > 9, long < 31)
      
      # Filter safety data for the selected date
      safety_map_data <- atmos_forecast_results() |> 
        filter(as.Date(time) == as.Date(input$selected_date)) 
      
      # Apply time filtering if a specific time is selected
      if (input$selected_time != "all_day") {
        safety_map_data <- safety_map_data |>
          filter(format(time, "%H:%M:%S") == input$selected_time)
      }
      
      # Join with route points and categorize using Beaufort Scale
      safety_map_data <- safety_map_data |> 
        semi_join(atmos_points$route, by = c("longitude", "latitude")) |> 
        mutate(
          safety_label = factor(
            safety_label, 
            levels = c("Safe", "Risky", "Unsafe")
          ),
          # Beaufort Scale categories
          beaufort_category = case_when(
            predicted_wind_speed < 0.3 ~ "0 (Calm)",
            predicted_wind_speed >= 0.3 & predicted_wind_speed < 1.5 ~ "1 (Light Air)",
            predicted_wind_speed >= 1.5 & predicted_wind_speed < 3.3 ~ "2 (Light Breeze)",
            predicted_wind_speed >= 3.3 & predicted_wind_speed < 5.5 ~ "3 (Gentle Breeze)",
            predicted_wind_speed >= 5.5 & predicted_wind_speed < 8.0 ~ "4 (Moderate Breeze)",
            predicted_wind_speed >= 8.0 & predicted_wind_speed < 10.8 ~ "5 (Fresh Breeze)",
            predicted_wind_speed >= 10.8 & predicted_wind_speed < 13.9 ~ "6 (Strong Breeze)",
            predicted_wind_speed >= 13.9 & predicted_wind_speed < 17.2 ~ "7 (Near Gale)",
            predicted_wind_speed >= 17.2 & predicted_wind_speed < 20.7 ~ "8 (Fresh Gale)",
            predicted_wind_speed >= 20.7 & predicted_wind_speed < 24.5 ~ "9 (Strong Gale)",
            predicted_wind_speed >= 24.5 & predicted_wind_speed < 28.4 ~ "10 (Storm)",
            predicted_wind_speed >= 28.4 & predicted_wind_speed < 32.6 ~ "11 (Violent Storm)",
            predicted_wind_speed >= 32.6 ~ "12 (Hurricane)"
          )
        )
      
      # Create interactive map
      p <- ggplot() +
        # Add Baltic Sea map as background
        geom_polygon(data = baltic_map, aes(x = long, y = lat, group = group),
                     fill = "gray80", color = "black") +
        # Add interactive hexbin safety data
        geom_hex_interactive(
          data = safety_map_data,
          aes(
            x = longitude, y = latitude, 
            fill = safety_label,
            tooltip = paste0("Wind Speed: ", predicted_wind_speed, " m/s\n",
                             "Beaufort: ", beaufort_category, "\n",
                             "Safety: ", safety_label),
            data_id = paste(longitude, latitude)
          ),
          bins = 40  # Adjust bin size for granularity
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
          title = paste("Interactive Safety Map (",
                        input$selected_date,
                        ifelse(input$selected_time != "all_day", paste("at", input$selected_time), ""),
                        ")")
        ) +
        theme_minimal() +
        theme(
          legend.title = element_text(size = 12),  # Larger legend title
          legend.text = element_text(size = 10),   # Larger legend text
          legend.key.size = unit(1, "cm")        # Larger legend keys
        )
      
      # Wrap the ggplot object in a girafe interactive object
      girafe(ggobj = p, options = list(
        opts_hover(css = "fill:orange;"),       # Change color on hover
        opts_hover_inv(css = "opacity:0.5;"),   # Lower opacity for non-hovered areas
        opts_tooltip(css = "font-size: 12px;")  # Tooltip styling
      ))
    })
  })  

# Route selection tab 0.5 degrees -----------------------------------------
  
  # Define variable groups for the Route Tab
  route_variable_groups <- list(
    basic = c("wind_speed", "significant_height_combined_waves_swell", "sea_ice_concentration"),
    extended = c("wind_speed", "significant_height_combined_waves_swell", "sea_ice_concentration",
                 "mean_wave_period", "sea_surface_temperature", "mean_sea_level_pressure",
                 "surface_pressure", "max_wind_gust", "instantaneous_wind_gust", "low_cloud_cover"),
    all = colnames(baltic_data)[sapply(baltic_data, is.numeric) &
                                  !colnames(baltic_data) %in% c("longitude", "latitude")]
  )
  
  # Reactive expression for selected Route Tab variables
  route_selected_variables <- reactive({
    req(input$route_variable_group)  # Ensure the user has made a selection
    route_variable_groups[[input$route_variable_group]]
  })
  
  # Reactive storage for points on the route
  route_points <- reactiveValues(
    data = tibble(type = character(), longitude = numeric(), latitude = numeric()),
    route = NULL,
    route_saved = FALSE
  )
  
  # Observe map clicks for route selection
  observeEvent(input$route_map_click, {
    click <- input$route_map_click
    if (!is.null(click)) {
      new_point <- tibble(
        type = input$route_point_type,  # Input for "Start", "Via", "End"
        longitude = click$lng,
        latitude = click$lat
      )
      route_points$data <- bind_rows(route_points$data, new_point)
      
      leafletProxy("route_map") |>
        addCircleMarkers(
          lng = click$lng,
          lat = click$lat,
          color = ifelse(input$route_point_type == "Start", "green",
                         ifelse(input$route_point_type == "End", "red", "orange")),
          label = paste(input$route_point_type, "(", round(click$lng, 2), ",", round(click$lat, 2), ")")
        )
    }
  })
  
  # Clear all points
  observeEvent(input$route_clear_route, {
    route_points$data <- tibble(type = character(), longitude = numeric(), latitude = numeric())
    route_points$route <- NULL
    leafletProxy("route_map") |> clearMarkers() |> clearShapes()
    output$route_saved_message <- renderText("Route cleared.")
  })
  
  # Plot the route
  observeEvent(input$route_plot_route, {
    if (nrow(route_points$data) < 2) {
      output$route_saved_message <- renderText("At least a Start and End point are required.")
      return(NULL)
    }
    
    # Sort points: Start -> Via -> End
    sorted_points <- route_points$data |>
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
        longitude = round(longitude * 2) / 2, # Round to nearest 0.5
        latitude = round(latitude * 2) / 2
      ) |>
      distinct()
    
    route_points$route <- route
    
    leafletProxy("route_map") |>
      clearShapes() |>
      addPolylines(data = route, lng = ~longitude, lat = ~latitude, color = "blue")
    
    output$route_saved_message <- renderText("Route plotted.")
  })
  
  # Save selected points for the route tab
  observeEvent(input$route_save_points, {
    if (nrow(route_points$data) > 0) {
      route_points$saved_points <- route_points$data |> as_tibble()
      route_points$route_saved <- TRUE  # Mark the route as saved
      output$route_saved_message <- renderText("Selected points have been saved as a tibble.")
    } else {
      output$route_saved_message <- renderText("No points to save.")
    }
  })
  
  # Display the raw points as a table
  output$route_table <- renderDT({
    req(route_points$route)  # Ensure data exists
    datatable(
      route_points$route,
      options = list(
        pageLength = 5,
        dom = 'tp',
        scrollX = TRUE
      )
    )
  })
  
  # Initialize the Leaflet map
  output$route_map <- renderLeaflet({
    leaflet() |> addTiles() |> setView(lng = 18.5, lat = 55.5, zoom = 6)
  })
  
  # Enable/disable "Run Predictions" button based on saved points
  observe({
    if (!is.null(route_points$saved_points) && nrow(route_points$saved_points) > 0) {
      shinyjs::enable("route_start_prediction")
      shinyjs::hide("route_prediction_warning")
    } else {
      shinyjs::disable("route_start_prediction")
      shinyjs::show("route_prediction_warning")
    }
  })
  
  # Reactive storage for forecast results (for the route tab)
  route_forecast_results <- reactiveVal(NULL)
  
  # Observe "Run Prediction" button click
  observeEvent(input$route_start_prediction, {
    req(route_selected_variables())
    req(route_points$route)
    
    # Validate the prediction date range
    if (as.Date(input$route_prediction_date_range[1]) > as.Date(input$route_prediction_date_range[2])) {
      showNotification("Start date cannot be later than end date!", type = "error")
      return()
    }
    
    print("Using selected route for predictions...")
    print("Starting predictions...")  # Debugging message
    
    # Get the selected variables
    selected_vars <- route_selected_variables()
    
    # Get the prediction date range
    prediction_dates <- seq.Date(
      from = as.Date(input$route_prediction_date_range[1]),
      to = as.Date(input$route_prediction_date_range[2]),
      by = "day"
    )
    
    future_times <- as.POSIXct(
      paste(
        rep(prediction_dates, each = 3),
        rep(c("06:00:00", "12:00:00", "18:00:00"), times = length(prediction_dates))
      ),
      tz = "UTC"
    )
    
    # Filter training data based on the route
    training_data <- baltic_data |>
      filter(
        latitude %in% route_points$route$latitude,
        longitude %in% route_points$route$longitude,
        year(time) >= 2012 & year(time) < 2014
      )
    print("Training data filtered.")
    
    # Group by latitude and longitude
    grouped_data <- training_data |>
      group_by(latitude, longitude) |>
      group_split()
    
    # Forecast parameters
    window_size <- 3
    future_length <- length(future_times)
    
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
          pull(var_name) |> 
          na.approx(rule = 2)  # Interpolate missing values
        
        # Initialize the forecast array
        forecasts <- numeric(future_length)
        
        # Handle predictions using predicted values iteratively
        for (j in seq_along(forecasts)) {
          recent_values <- c(ts_data, forecasts[1:(j - 1)])
          recent_values <- na.approx(recent_values, rule = 2)
          
          if (length(recent_values) < window_size) {
            forecasts[j] <- NA
          } else if (var_name %in% c("significant_height_combined_waves_swell")) {
            # Use SMA for wave height stability
            sma_result <- SMA(recent_values, n = 9)  # Smooth over 3-day periods
            forecasts[j] <- tail(sma_result, 1)
          } else if (var_name %in% c("wind_speed", "sea_ice_concentration")) {
            # Use EMA for responsive variables
            ema_result <- EMA(recent_values, n = 18, alpha = 0.3)  # Moderate smoothing
            forecasts[j] <- tail(ema_result, 1)
          } else {
            # Default to EMA for other variables
            ema_result <- EMA(recent_values, n = 36, alpha = 0.2)  # Long-term smoothing
            forecasts[j] <- tail(ema_result, 1)
          }
        }
        
        # Store the forecasts in the forecast list
        forecast_list[[v]] <- tibble(!!paste0("predicted_", var_name) := forecasts)
      }
      
      # Combine all predictions for this location
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
    route_forecast_results(bind_rows(results))
    print("Predictions complete.")
  })
  
  # Assign safety labels to forecast results
  observeEvent(input$route_start_prediction, {
    req(route_forecast_results())
    
    updated_results <- route_forecast_results() |> 
      mutate(
        safety_label = case_when(
          predicted_wind_speed >= 10.8 ~ "Unsafe",
          predicted_significant_height_combined_waves_swell >= 3 ~ "Unsafe",
          predicted_sea_ice_concentration > 0.3 ~ "Risky",
          predicted_wind_speed >= 8.0 & predicted_wind_speed < 10.8 ~ "Risky",
          TRUE ~ "Safe"
        )
      )
    
    route_forecast_results(updated_results)
    print("Safety labels assigned.")
  })
  
  # Render safety map
  observeEvent(input$route_change_visualization, {
    output$safety_map <- renderPlot({
      req(route_forecast_results())
      req(input$route_selected_date)
      
      # Get the world map and filter for the Baltic Sea region
      baltic_map <- map_data("world") |>
        filter(lat > 53, lat < 66, long > 9, long < 31)
      
      # Filter safety data for the selected date
      safety_map_data <- route_forecast_results() |> 
        filter(as.Date(time) == as.Date(input$route_selected_date))
      
      # Apply time filtering if a specific time is selected
      if (input$route_selected_time != "all_day") {
        safety_map_data <- safety_map_data |>
          filter(format(time, "%H:%M:%S") == input$route_selected_time)
      }
      
      # Join with route points
      safety_map_data <- safety_map_data |> 
        semi_join(route_points$route, by = c("longitude", "latitude")) |> 
        mutate(
          safety_label = factor(
            safety_label,
            levels = c("Safe", "Risky", "Unsafe")
          )
        )
      
      # Create the map with safety labels
      ggplot() +
        geom_polygon(data = baltic_map, aes(x = long, y = lat, group = group),
                     fill = "gray80", color = "black") +
        geom_hex(
          data = safety_map_data,
          aes(x = longitude, y = latitude, fill = safety_label),
          bins = 20
        ) +
        scale_fill_paletteer_d(
          "ggsci::light_blue_material",
          name = "Safety Label",
          na.value = "gray"
        ) +
        coord_quickmap() +
        labs(
          x = "Longitude",
          y = "Latitude",
          title = paste("Safety Labels Along the Route (", 
                        input$route_selected_date, 
                        ifelse(input$route_selected_time != "all_day", paste("at", input$route_selected_time), ""), 
                        ")", sep = "")
        ) +
        theme_minimal() +
        theme(
          legend.title = element_text(size = 14),
          legend.text = element_text(size = 12),
          legend.key.size = unit(1.2, "cm")
        )
    })
  })
  
  # Render prediction results table
  output$route_prediction_table <- renderDT({
    req(route_forecast_results())
    
    datatable(
      route_forecast_results(),
      options = list(
        pageLength = 10,
        scrollX = TRUE,
        dom = 'tp'
      )
    )
  })
  
  # Clear all for the Route tab
  observeEvent(input$route_clear_all, {
    # Clear route data
    route_points$data <- tibble(type = character(), longitude = numeric(), latitude = numeric())
    route_points$route <- NULL
    route_points$route_saved <- FALSE
    leafletProxy("route_map") |> clearMarkers() |> clearShapes()
    
    # Clear prediction results
    route_forecast_results(NULL)
    
    # Reset UI inputs
    updateDateRangeInput(session, "route_prediction_date_range", 
                         start = "2025-01-01", end = "2025-01-31")
    updateDateInput(session, "route_selected_date", value = Sys.Date())
    updateSelectInput(session, "route_selected_time", selected = "all_day")
    shinyjs::reset("route_variable_group")
    
    showNotification("All inputs and predictions cleared.", type = "message")
  })

# Plot tab ----------------------------------------------------------------
  
  # Reactive expression to filter the data based on the selected time range
  filtered_data <- reactive({
    baltic_data |>
      filter(as.Date(time) >= input$time_range[1] & as.Date(time) <= input$time_range[2])
  })
  
  # Render the plot
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
}

# Run the App
shinyApp(ui = ui, server = server)
