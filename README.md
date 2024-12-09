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
  Uses Weighted Moving Average (WMA) techniques to forecast weather conditions.

- **Interactive R Shiny Application**  
  - Allows users to select start and end points for a sailing journey.  
  - Visualizes safety recommendations and weather forecasts along the route.
