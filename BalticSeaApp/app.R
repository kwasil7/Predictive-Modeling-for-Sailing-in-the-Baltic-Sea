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
library(ggiraph)

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
        selectInput(
          "atmos_sailor_experience", 
          "Sailor Experience Level:",
          choices = c("Beginner" = "beginner", "Intermediate/Advanced" = "advanced"),
          selected = "beginner"
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
          id = "atmos_prediction_warning",
          style = "color: red; font-weight: bold;",
          "Please save the route before running predictions."
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
        
        h3("Interactive Visualization"),
        conditionalPanel(
          condition = "input.atmos_variable_group == 'all_atmos'",
          checkboxInput("show_interactive_plot", "Show Interactive Plot", value = TRUE),
          selectInput(
            "selected_variable",
            "Select Variable to Display:",
            choices = c(
              "Wind Speed [m/s]" = "predicted_wind_speed",
              "Mean Sea Level Pressure [hPa]" = "predicted_mean_sea_level_pressure",
              "Sea Surface Temperature [°C]" = "predicted_sea_surface_temperature",
              "Surface Pressure" = "predicted_surface_pressure",
              "Max Wind Gust [m/s]" = "predicted_max_wind_gust",
              "Instantaneous Wind Gust [m/s]" = "predicted_instantaneous_wind_gust",
              "Low Cloud Cover" = "predicted_low_cloud_cover",
              "Total Cloud Cover" = "predicted_total_cloud_cover",
              "Precipitation Type" = "predicted_precipitation_type",
              "Convective Available Potential Energy" = "predicted_convective_available_potential_energy",
              "Sea Ice Concentration" = "predicted_sea_ice_concentration"
            ),
            selected = "predicted_wind_speed"
          )
        ),
        
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
        
        hr(),
        
        div(id = "atmos_additional_plots", style = "display: none;",
            h3("Variables Insights"),
            plotOutput("atmos_wind_speed_plot", height = "400px"),
            plotOutput("atmos_sea_ice_plot", height = "400px")
        ),
        
        conditionalPanel(
          condition = "input.atmos_variable_group == 'all_atmos' && input.show_interactive_plot == true",
          h3("Interactive Plot for All Variables"),
          plotOutput("atmos_interactive_plot", height = "600px", width = "800px")
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
            "Basic variables (safety-related only)" = "basic",
            "Extended variables (all)" = "extended"
          ),
          selected = "basic"
        ),
        selectInput(
          "route_sailor_experience", 
          "Sailor Experience Level:",
          choices = c("Beginner" = "beginner", "Intermediate/Advanced" = "advanced"),
          selected = "beginner"
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
        
        h3("Interactive Visualization"),
        conditionalPanel(
          condition = "input.route_variable_group == 'extended'",
          checkboxInput("route_show_interactive_plot", "Show Interactive Plot", value = TRUE),
          selectInput(
            "route_selected_variable",
            "Select Variable to Display:",
            choices = c(
              "Wind Gust Factor" = "predicted_wind_gust_factor",
              "Wind Speed [m/s]" = "predicted_wind_speed",
              "Mean Wave Direction [°]" = "predicted_mean_wave_direction",
              "Mean Wave Period [s]" = "predicted_mean_wave_period",
              "Significant Height of Combined Waves and Swell [m]" = "predicted_significant_height_combined_waves_swell",
              "Max Individual Wave Height [m]" = "predicted_max_individual_wave_height",
              "Peak Wave Period [s]" = "predicted_peak_wave_period",
              "Significant Height of Wind Waves [m]" = "predicted_significant_height_of_wind_waves",
              "Mean Sea Level Pressure [hPa]" = "predicted_mean_sea_level_pressure",
              "Sea Surface Temperature [°C]" = "predicted_sea_surface_temperature",
              "Surface Pressure [hPa]" = "predicted_surface_pressure",
              "Max Wind Gust [m/s]" = "predicted_max_wind_gust",
              "Instantaneous Wind Gust [m/s]" = "predicted_instantaneous_wind_gust",
              "Low Cloud Cover [%]" = "predicted_low_cloud_cover",
              "Total Cloud Cover [%]" = "predicted_total_cloud_cover",
              "Convective Available Potential Energy [J/kg]" = "predicted_convective_available_potential_energy",
              "Sea Ice Concentration [0-1]" = "predicted_sea_ice_concentration",
              "Wind Direction [°]" = "predicted_wind_direction",
              "K Index" = "predicted_k_index",
              "Total Totals Index" = "predicted_total_totals_index",
              "Wavelength [m]" = "predicted_wavelength"
            ),
            selected = "predicted_wind_speed"
          )
        ),
        
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
        
        h3("Interactive Map"),
        girafeOutput("route_interactive_map", height = "800px", width = "1200px"),
        
        hr(),
        
        conditionalPanel(
          condition = "input.route_variable_group == 'extended' && input.route_show_interactive_plot == true",
          h3("Interactive Plot for All Variables"),
          plotOutput("route_interactive_plot", height = "600px", width = "800px")
        ),
        
        hr(),
        
        h3("Prediction Results"),
        DTOutput("route_prediction_table"),
        
        hr(),
        
        div(id = "route_additional_plots", style = "display: none;",
            h3("Variables Insights"),
            plotOutput("route_wind_speed_plot", height = "400px"),
            plotOutput("route_wave_height_plot", height = "400px"),
            plotOutput("route_sea_ice_plot", height = "400px")
        )
      )
    )
  ),
  
  # Tab 3: Historical Analysis
  tabPanel(
    "Historical Analysis",
    sidebarLayout(
      sidebarPanel(
        useShinyjs(),
        h4("Explore Historical Data"),
        helpText(
          "This tab is intended for the results of exploratory data analysis (EDA) ",
          "of the dataset provided by the Copernicus Climate Data Store: ",
          a("ERA5 Reanalysis Single Levels", 
            href = "https://cds.climate.copernicus.eu/datasets/reanalysis-era5-single-levels?tab=overview",
            target = "_blank"),
          ". Select variables and time ranges below to visualize historical trends."
        ),
        hr(),
        selectInput("plot_var", "Select Variable to Plot:",
                    choices = c("Wind speed" = "wind_speed",
                                "Wind gust factor" = "wind_gust_factor",
                                "Wave height" = "significant_height_combined_waves_swell",
                                "Mean wave period" = "mean_wave_period",
                                "Max individual wave height" = "max_individual_wave_height",
                                "Peak wave period" = "peak_wave_period",
                                "Sea surface temperature" = "sea_surface_temperature")),
        sliderInput("time_range", "Time Range:",
                    min = as.Date("2012-01-01"),
                    max = as.Date("2013-12-31"),
                    value = c(as.Date("2012-01-01"), as.Date("2013-12-31")),
                    timeFormat = "%Y-%m-%d"),
        hr(),
        checkboxInput("show_heatmap", "Show Interactive Heatmap", value = FALSE),
        
        # Conditional dropdowns for X and Y axis variables
        conditionalPanel(
          condition = "input.show_heatmap == true",
          selectInput("heatmap_x", "Select X-axis Variable:",
                      choices = c("Beaufort Category" = "beaufort_category",
                                  "Douglas Category" = "douglas_category",
                                  "Oktas Category" = "oktas_category",
                                  "Season" = "season",
                                  "Safety Label" = "safety_label",
                                  "Precipitation Type" = "precipitation_type")),
          selectInput("heatmap_y", "Select Y-axis Variable:",
                      choices = c("Douglas Category" = "douglas_category",
                                  "Beaufort Category" = "beaufort_category",
                                  "Oktas Category" = "oktas_category",
                                  "Season" = "season",
                                  "Safety Label" = "safety_label",
                                  "Precipitation Type" = "precipitation_type"))
        )
      ),
      mainPanel(
        h3("Variable Trends"),
        plotOutput("plot_output"),
        hr(),
        h4("Summary Statistics"),
        verbatimTextOutput("plot_summary"),
        hr(),
        conditionalPanel(
          condition = "input.show_heatmap == true",
          h4("Interactive Heatmap"),
          plotOutput("heatmap_output", height = "600px", width = "800px")
        ),
        hr(),
        h4("Saved Historical Plots"),
        p("Below are the pre-generated plots from the historical analysis:"),
        
        # Adjust the layout for centering and enlarging images
        fluidRow(
          column(8, offset = 2,  # Center column (offset = 2 moves it to the center)
                 img(src = "2_plots_max_wave_height_and_others.png", height = "400px", width = "100%"))
        ),
        fluidRow(
          column(8, offset = 2,
                 img(src = "air_density_vs_sea_surface_temp.png", height = "400px", width = "100%"))
        ),
        fluidRow(
          column(8, offset = 2,
                 img(src = "mean_wave_period_by_douglas.png", height = "400px", width = "100%"))
        ),
        fluidRow(
          column(8, offset = 2,
                 img(src = "sea_surface_temp_by_precipitation_type.png", height = "400px", width = "100%"))
        ),
        fluidRow(
          column(8, offset = 2,
                 img(src = "sea_surface_temperature_over_time.png", height = "400px", width = "100%"))
        ),
        fluidRow(
          column(8, offset = 2,
                 img(src = "surface_pressure_and_wind_gust_by_beaufort.png", height = "400px", width = "100%"))
        ),
        fluidRow(
          column(8, offset = 2,
                 img(src = "time_max_wave_height.png", height = "400px", width = "100%"))
        ),
        fluidRow(
          column(8, offset = 2,
                 img(src = "wave_height_over_time.png", height = "400px", width = "100%"))
        ),
        fluidRow(
          column(8, offset = 2,
                 img(src = "wavelength_to_wind_speed.png", height = "400px", width = "100%"))
        ),
        fluidRow(
          column(8, offset = 2,
                 img(src = "wind_speed_to_max_wave_height_by_oktas.png", height = "400px", width = "100%"))
        ),
        fluidRow(
          column(8, offset = 2,
                 img(src = "windrose.png", height = "400px", width = "100%"))
        )
      )
    )
  )
)

library(zoo)
library(TTR)
library(paletteer)
library(forecast)
library(stats)

# Use in a ggplot2 chart:
scale_colour_paletteer_d("ggsci::light_blue_material")
scale_fill_paletteer_d("ggsci::light_blue_material")

scale_color_paletteer_c("ggthemes::Blue-Teal")
scale_fill_paletteer_c("ggthemes::Blue-Teal")

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
    
    output$atmos_saved_message <- renderText("Prediction has started. Please wait for the results...")
    
    req(atmos_selected_variables())
    req(atmos_points$route)
    req(atmos_points$route_saved)
    
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
        
        if (length(ts_data) < 2) {
          # Not enough data to fit a time series model
          forecast_list[[v]] <- tibble(!!paste0("predicted_", var_name) := rep(NA, future_length))
          next
        }
        
        if (var_name %in% c("low_cloud_cover", "total_cloud_cover")) {
          # Use EMA for cloud variables
          future_forecast <- tryCatch({
            ema_forecast <- EMA(ts_data, ratio = 0.1, wilder = TRUE)  # Adjust ratio as needed
            rep(tail(ema_forecast, 1), future_length)  # Repeat the last EMA value for the forecast length
          }, error = function(e) {
            message(sprintf("EMA failed for %s: %s", var_name, e$message))
            rep(NA, future_length)
          })
        } else {
          # Create time series object for Holt-Winters
          ts_obj <- ts(ts_data, frequency = 1095)  # Adjust frequency for yearly seasonality
          
          # Fit Holt-Winters model
          hw_model <- tryCatch({
            HoltWinters(ts_obj)
          }, error = function(e) {
            # Handle model fitting errors gracefully
            message(sprintf("Holt-Winters failed for %s: %s", var_name, e$message))
            NULL
          })
          
          if (is.null(hw_model)) {
            # If model fitting fails, return NA forecasts
            forecast_list[[v]] <- tibble(!!paste0("predicted_", var_name) := rep(NA, future_length))
            next
          }
          
          # Generate forecasts for the specified future length
          future_forecast <- tryCatch({
            predict(hw_model, n.ahead = future_length)
          }, error = function(e) {
            # Handle prediction errors gracefully
            message(sprintf("Forecasting failed for %s: %s", var_name, e$message))
            rep(NA, future_length)
          })
        }
  
  # Store the forecasts
  forecast_list[[v]] <- tibble(!!paste0("predicted_", var_name) := as.numeric(future_forecast))
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
  
  # Add safety labels to atmospheric forecast results based on sailor experience
  observeEvent(input$atmos_predict, {
    # Ensure atmospheric forecast results exist
    req(atmos_forecast_results())
    
    # Determine experience level from input
    experience_level <- input$atmos_sailor_experience # 'beginner' or 'advanced'
    
    # Add safety labels to the forecast results
    updated_results <- atmos_forecast_results() |> 
      mutate(
        safety_label = case_when(
          # Logic for Beginner Sailors (more conservative thresholds)
          experience_level == "beginner" & predicted_wind_speed >= 5.5 & predicted_wind_speed < 8.0 ~ "Risky",
          experience_level == "beginner" & predicted_wind_speed >= 8.0 ~ "Unsafe",
          experience_level == "beginner" & predicted_sea_ice_concentration > 0.15 ~ "Unsafe",
          
          # Logic for Intermediate/Advanced Sailors
          predicted_wind_speed >= 10.8 ~ "Unsafe",
          predicted_sea_ice_concentration > 0.3 ~ "Risky",
          predicted_wind_speed >= 8.0 & predicted_wind_speed < 10.8 ~ "Risky",
          
          # Safe for all levels
          TRUE ~ "Safe"
        )
      )
    
    # Update the reactiveVal with safety labels
    atmos_forecast_results(updated_results)
  })
  
  
  # Enable/disable prediction button based on route_saved
  observe({
    if (isTRUE(atmos_points$route_saved)) {
      shinyjs::enable("atmos_predict")
      shinyjs::hide("atmos_prediction_warning")
    } else {
      shinyjs::disable("atmos_predict")
      shinyjs::show("atmos_prediction_warning")
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
    if (input$atmos_variable_group == "basic") {
      shinyjs::show("atmos_additional_plots")
    } else {
      shinyjs::hide("atmos_additional_plots")
    }
  })
  
  # Wind Speed Plot
  output$atmos_wind_speed_plot <- renderPlot({
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
  
  # Sea ice concentration plot
  output$atmos_sea_ice_plot <- renderPlot({
    req(atmos_forecast_results())
    
    atmos_forecast_results() |>
      ggplot(aes(x = time, y = predicted_sea_ice_concentration)) +
      geom_line(color = "blue") +
      geom_smooth(se = FALSE, color = "aquamarine") +
      labs(
        x = "Time",
        y = "Sea ice concentration [0; 1]",
        title = "Predicted sea ice concentration over time"
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
                        ifelse(input$selected_time != "all_day", paste(" at", input$selected_time), ""),
                        ")")
        ) +
        theme_minimal() +
        theme(
          legend.title = element_text(size = 10),  # Larger legend title
          legend.text = element_text(size = 8),   # Larger legend text
          legend.key.size = unit(0.5, "cm")        # Larger legend keys
        )
      
      # Wrap the ggplot object in a girafe interactive object
      interactive_map <- girafe(ggobj = p)
      
      # Add zoom capability
      interactive_map <- girafe_options(interactive_map,
                                        opts_hover(css = "fill:orange;"),       # Change color on hover
                                        opts_hover_inv(css = "opacity:0.5;"),   # Lower opacity for non-hovered areas
                                        opts_tooltip(css = "font-size: 14px;"), # Tooltip styling
                                        opts_zoom(min = 0.5, max = 5)           # Enable zoom with limits
      )
      
      # Return the interactive map
      interactive_map
    })
  })

# Interactive plot for all atmospheric variables --------------------------

  output$atmos_interactive_plot <- renderPlot({
    req(input$show_interactive_plot)  # Ensure the checkbox is checked
    req(atmos_forecast_results())    # Ensure forecast results exist
    
    # Ensure that "all_atmos" is selected
    req(input$atmos_variable_group == "all_atmos")
    
    # Dynamically select the variable to plot based on user input
    variable_to_plot <- input$selected_variable
    
    # Preprocess data for specific variable conversions
    processed_data <- atmos_forecast_results() |>
      mutate(
        predicted_sea_surface_temperature = predicted_sea_surface_temperature - 273.15,
        predicted_mean_sea_level_pressure = predicted_mean_sea_level_pressure / 100,
        predicted_surface_pressure = predicted_surface_pressure / 100
      )
    
    # Generate the interactive plot
    processed_data |>
      ggplot(aes(x = time, y = .data[[variable_to_plot]])) +
      geom_line(size = 1, color = "blue") +
      labs(
        x = "Time",
        y = variable_to_plot,
        title = paste("Predicted", variable_to_plot, "over time")
      ) +
      scale_color_paletteer_c("ggthemes::Blue-Teal", name = "Variable") +
      theme_minimal() +
      theme(legend.position = "top")
  })
  
# Route selection tab 0.5 degrees -----------------------------------------
  
  # Define variable groups for the Route Tab
  route_variable_groups <- list(
    basic = c("wind_speed", "significant_height_combined_waves_swell", "sea_ice_concentration"),
    extended = c(
      "wind_gust_factor",
      "wind_speed",
      "mean_wave_direction",
      "mean_wave_period",
      "significant_height_combined_waves_swell",
      "max_individual_wave_height",
      "peak_wave_period",
      "significant_height_of_wind_waves",
      "mean_sea_level_pressure",
      "sea_surface_temperature",
      "surface_pressure",
      "max_wind_gust",
      "instantaneous_wind_gust",
      "low_cloud_cover",
      "total_cloud_cover",
      "convective_available_potential_energy",
      "sea_ice_concentration",
      "wind_direction",
      "k_index",
      "total_totals_index",
      "wavelength"
    )
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
    output$route_saved_message <- renderText("Prediction has started. Please wait for the results...")
    
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
        
        if (length(ts_data) < 2) {
          # Not enough data to fit a time series model
          forecast_list[[v]] <- tibble(!!paste0("predicted_", var_name) := rep(NA, future_length))
          next
        }
        
        if (var_name %in% c("low_cloud_cover", "total_cloud_cover")) {
          # Use EMA for cloud variables
          future_forecast <- tryCatch({
            ema_forecast <- EMA(ts_data, ratio = 0.01, wilder = TRUE)  # Adjust ratio as needed
            rep(tail(ema_forecast, 1), future_length)  # Repeat the last EMA value for the forecast length
          }, error = function(e) {
            message(sprintf("EMA failed for %s: %s", var_name, e$message))
            rep(NA, future_length)
          })
        } else {
          # Use Holt-Winters for other variables
          future_forecast <- tryCatch({
            ts_obj <- ts(ts_data, frequency = 1095)  # Adjust frequency for yearly seasonality
            hw_model <- HoltWinters(ts_obj)
            predict(hw_model, n.ahead = future_length)
          }, error = function(e) {
            message(sprintf("Holt-Winters failed for %s: %s", var_name, e$message))
            rep(NA, future_length)
          })
        }
        
        # Store the forecasts
        forecast_list[[v]] <- tibble(!!paste0("predicted_", var_name) := as.numeric(future_forecast))
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
  
  # Assign safety labels to route forecast results based on sailor experience
  observeEvent(input$route_start_prediction, {
    req(route_forecast_results())
    
    # Determine experience level from input
    experience_level <- input$route_sailor_experience  # 'beginner' or 'advanced'
    
    # Add safety labels to the forecast results
    updated_results <- route_forecast_results() |> 
      mutate(
        safety_label = case_when(
          # Conditions for Beginner Sailors (conservative thresholds)
          experience_level == "beginner" & predicted_wind_speed >= 5.5 & predicted_wind_speed < 8.0 ~ "Risky",
          experience_level == "beginner" & predicted_wind_speed >= 8.0 ~ "Unsafe",
          experience_level == "beginner" & predicted_significant_height_combined_waves_swell >= 1.25 & 
            predicted_significant_height_combined_waves_swell < 3.0 ~ "Risky",
          experience_level == "beginner" & predicted_significant_height_combined_waves_swell >= 3.0 ~ "Unsafe",
          experience_level == "beginner" & predicted_sea_ice_concentration > 0.15 ~ "Unsafe",
          
          # Conditions for Intermediate/Advanced Sailors
          predicted_wind_speed >= 10.8 ~ "Unsafe",
          predicted_significant_height_combined_waves_swell >= 2.5 ~ "Unsafe",
          predicted_sea_ice_concentration > 0.3 ~ "Risky",
          predicted_wind_speed >= 8.0 & predicted_wind_speed < 10.8 ~ "Risky",
          
          # Safe for all levels
          TRUE ~ "Safe"
        )
      )
    
    # Update the reactiveVal with safety labels
    route_forecast_results(updated_results)
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
          bins = 25
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
  
  # Show/Hide Additional Plots Based on Prediction Variables
  observe({
    if (input$route_variable_group == "basic") {
      shinyjs::show("route_additional_plots")
    } else {
      shinyjs::hide("route_additional_plots")
    }
  })
  
  # Wind Speed Plot
  output$route_wind_speed_plot <- renderPlot({
    req(route_forecast_results())
    
    route_forecast_results() |>
      ggplot(aes(x = time, y = predicted_wind_speed)) +
      geom_line(color = "blue") +
      geom_smooth(se = FALSE, color = "red") +
      labs(
        x = "Time", 
        y = "Wind speed [m/s]",
        title = "Predicted wind speed over time"
      ) +
      theme_minimal()
  })
  
  # Wave height plot
  output$route_wave_height_plot <- renderPlot({
    req(route_forecast_results())
    
    route_forecast_results() |>
      ggplot(aes(x = time, y = predicted_significant_height_combined_waves_swell)) +
      geom_line(color = "darkgreen") +
      geom_smooth(se = FALSE, color = "orange") +
      labs(
        x = "Time",
        y = "Wave height [m]",
        title = "Predicted significant height of combined waves and swell over time"
      ) +
      theme_minimal()
  })
  
  # Sea ice concentration plot
  output$route_sea_ice_plot <- renderPlot({
    req(route_forecast_results())
    
    route_forecast_results() |>
      ggplot(aes(x = time, y = predicted_sea_ice_concentration)) +
      geom_line(color = "blue") +
      geom_smooth(se = FALSE, color = "aquamarine") +
      labs(
        x = "Time",
        y = "Sea ice concentration [0; 1]",
        title = "Predicted sea ice concentration over time"
      ) +
      theme_minimal()
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
  

# Route interactive map ---------------------------------------------------

  # Observe "Create Interactive Plot" button click
  observeEvent(input$route_change_visualization, {
    output$route_interactive_map <- renderGirafe({
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
      
      # Join with route points and categorize using Beaufort Scale and Douglas Scale
      safety_map_data <- safety_map_data |> 
        semi_join(route_points$route, by = c("longitude", "latitude")) |> 
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
          ),
          # Douglas Scale categories
          douglas_category = case_when(
            predicted_significant_height_combined_waves_swell == 0 ~ "0 Calm (glassy)",
            predicted_significant_height_combined_waves_swell > 0 & predicted_significant_height_combined_waves_swell <= 0.1 ~ "1 Calm (rippled)",
            predicted_significant_height_combined_waves_swell > 0.1 & predicted_significant_height_combined_waves_swell <= 0.5 ~ "2 Smooth (wavelets)",
            predicted_significant_height_combined_waves_swell > 0.5 & predicted_significant_height_combined_waves_swell <= 1.25 ~ "3 Slight",
            predicted_significant_height_combined_waves_swell > 1.25 & predicted_significant_height_combined_waves_swell <= 2.5 ~ "4 Moderate",
            predicted_significant_height_combined_waves_swell > 2.5 & predicted_significant_height_combined_waves_swell <= 4 ~ "5 Rough",
            predicted_significant_height_combined_waves_swell > 4 & predicted_significant_height_combined_waves_swell <= 6 ~ "6 Very rough",
            predicted_significant_height_combined_waves_swell > 6 & predicted_significant_height_combined_waves_swell <= 9 ~ "7 High",
            predicted_significant_height_combined_waves_swell > 9 & predicted_significant_height_combined_waves_swell <= 14 ~ "8 Very high",
            predicted_significant_height_combined_waves_swell > 14 ~ "9 Phenomenal"
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
                             "Douglas: ", douglas_category, "\n",
                             "Safety: ", safety_label),
            data_id = paste(longitude, latitude)
          ),
          bins = 25  # Adjust bin size for granularity
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
                        input$route_selected_date,
                        ifelse(input$route_selected_time != "all_day", paste(" at", input$route_selected_time), ""),
                        ")")
        ) +
        theme_minimal() +
        theme(
          legend.title = element_text(size = 10),  # Larger legend title
          legend.text = element_text(size = 8),   # Larger legend text
          legend.key.size = unit(0.5, "cm")        # Larger legend keys
        )
      
      # Wrap the ggplot object in a girafe interactive object
      interactive_map <- girafe(ggobj = p)
      
      # Add zoom capability
      interactive_map <- girafe_options(interactive_map,
                                        opts_hover(css = "fill:orange;"),       # Change color on hover
                                        opts_hover_inv(css = "opacity:0.5;"),   # Lower opacity for non-hovered areas
                                        opts_tooltip(css = "font-size: 14px;"), # Tooltip styling
                                        opts_zoom(min = 0.5, max = 5)           # Enable zoom with limits
      )
      
      # Return the interactive map
      interactive_map
    })
  })
  
# Route interactive plot for all variables --------------------------------

  # Interactive Plot for Route Tab
  output$route_interactive_plot <- renderPlot({
    req(input$route_show_interactive_plot)  # Ensure the checkbox is checked
    req(route_forecast_results())          # Ensure forecast results exist
    
    # Ensure that all variables are selected
    req(input$route_variable_group == "extended")
    
    # Dynamically select the variable to plot based on user input
    variable_to_plot <- input$route_selected_variable
    
    # Preprocess data for specific variable conversions
    processed_data <- route_forecast_results() |>
      mutate(
        predicted_mean_sea_level_pressure = predicted_mean_sea_level_pressure / 100,
        predicted_surface_pressure = predicted_surface_pressure / 100
      )
    
    # Generate the interactive plot
    processed_data |>
      ggplot(aes(x = time, y = .data[[variable_to_plot]])) +
      geom_line(color = "coral") +
      geom_smooth() +
      labs(
        x = "Time",
        y = variable_to_plot,
        title = paste(variable_to_plot, " over time")
      ) +
      scale_color_paletteer_c("ggthemes::Blue-Teal", name = "Variable") +
      theme_minimal() +
      theme(legend.position = "top")
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
  
  # Heatmap
  observeEvent(input$show_heatmap, {
    output$heatmap_output <- renderPlot({
      req(baltic_data)  # Ensure the dataset is loaded
      
      # Dynamically count data based on selected variables
      heatmap_data <- baltic_data |>
        count(!!sym(input$heatmap_x), !!sym(input$heatmap_y), name = "count")
      
      # Create heatmap
      ggplot(heatmap_data, aes(x = .data[[input$heatmap_x]], y = .data[[input$heatmap_y]])) +
        geom_tile(aes(fill = count), color = "white") +
        scale_fill_paletteer_c("ggthemes::Blue-Teal", name = "Count") +
        labs(
          x = input$heatmap_x,
          y = input$heatmap_y,
          title = paste("Heatmap of", input$heatmap_x, "and", input$heatmap_y)
        ) +
        theme_minimal() +
        theme(
          axis.text.x = element_text(angle = 45, hjust = 1),
          axis.text = element_text(size = 14)
        )
    })
  })
}

# Run the App
shinyApp(ui = ui, server = server)
