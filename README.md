# Predictive Modeling for Sailing Safety in the Baltic Sea

## Project Overview

This project uses predictive modeling techniques to analyze historical weather data and provide safety recommendations for predefined sailing routes in the Baltic Sea. The system leverages historical weather patterns, forecasts, and user input to assess the safety of specific routes.

The goal is to help sailors make informed decisions by predicting weather conditions along their planned routes and categorizing safety levels as **Safe**, **Risky**, or **Unsafe**.

---

## Data Source

The primary dataset used in this project is from the [Copernicus Climate Data Store](https://cds.climate.copernicus.eu/datasets/reanalysis-era5-single-levels?tab=overview), specifically the ERA5 Reanalysis dataset. It provides high-resolution weather data for the Baltic Sea region.

---

## Features

- **Route Safety Assessment**  
  Predicts the safety of a user-defined sailing route based on weather conditions like wind speed and wave height.

- **Weather Forecasts**  
  Uses Holt-Winters seasonal method and Arnaud Legoux moving average techniques to forecast weather conditions.

- **Interactive R Shiny Application**  
  - Allows users to select start and end points for a sailing journey.  
  - Visualizes safety recommendations and weather forecasts along the route.

## User Guide

**1. Accessing the Main Features**

  Select a Tab
  - At the top of the application, you will see several tabs.
  - Atmospheric tab: Offers a finer resolution (0.25°) but fewer variables.
  - Route tab: Provides 0.5° resolution but includes all available variables.

  Choose Your Points on the Map
  - Click anywhere on the displayed map to add route points.
  - You can add as many points as you like to outline your desired path.

  Generate and Save the Route
  - Once you have placed your points, click the “Plot Route” button to visualize your path.
  - To store these points for later, click the “Save Route Points” button.

**2. Setting Prediction Date Range**

  Select the Forecast Period
  - In the designated panel, choose the start and end dates for which you want predictions.
  - This date range determines the timeframe over which the application will predict conditions.

**3. Adjusting Visualization Settings**

  Within the Visualization Settings, make sure to select the date corresponding to your chosen date range.

**4. Interpreting Safety Maps**

  Safety Categories
  - Once the route is plotted, the application will generate safety maps.
  - The path will be segmented into Safe, Risky, or Unsafe sections based on the predicted atmospheric and marine conditions.

  Refine and Recalculate
  - If certain route segments are classified as Unsafe, consider adjusting the route points to avoid high-risk conditions.
  - Re-plot and re-save your route until you achieve a satisfactory safety profile.

Tip: Always ensure your Visualization Settings and Date Range align.

## The Shiny App <a href="https://kw888.shinyapps.io/The_Baltic_Sea_Safety_App/" target="_blank">here it is</a>

It will be available with full functionality until the 29th of January 2025.

