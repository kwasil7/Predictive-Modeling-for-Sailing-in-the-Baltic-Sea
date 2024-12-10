library(ncdf4)
library(tidync)
library(tidyverse)

# Old data ----------------------------------------------------------------

nc_data_physics_ice <- tidync("cmems_mod_bal_phy_my_P1D-m_multi-vars_9.04E-30.21E_53.01N-65.89N_2008-01-01-2009-12-31.nc")

physics_ice_data <- nc_data_physics_ice |>
  hyper_filter() |>
  hyper_tibble() |>
  select(sla, siconc, sithick, longitude, latitude, time) |>
  mutate(time = as.Date(time, origin = "1970-01-01"))

# Convert longitude and latitude to numeric
physics_ice_data <- physics_ice_data |> 
  mutate(
    longitude = as.numeric(longitude),
    latitude = as.numeric(latitude)
  )

# First summarize then round the results
physics_ice_data_reduced <- physics_ice_data |>
  group_by(longitude = round(longitude, 0),
           latitude = round(latitude, 0),
           time) |>
  summarize(
    sla = mean(sla, na.rm = TRUE),
    siconc = mean(siconc, na.rm = TRUE),
    sithick = mean(sithick, na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(
    sla = round(sla, 2),
    siconc = round(siconc, 2),
    sithick = round(sithick, 2)
  )

nc_data_physics_sea <- tidync("cmems_mod_bal_phy_my_P1D-m_thetao-uo-vo_9.04E-30.21E_53.01N-65.89N_0.50m_2008-01-01-2009-12-31.nc")

physics_sea_data <- nc_data_physics_sea |>
  hyper_filter() |>
  hyper_tibble() |>
  select(thetao, uo, vo, longitude, latitude, time) |>
  mutate(time = as.Date(time, origin = "1970-01-01"))

# Convert longitude and latitude to numeric
physics_sea_data <- physics_sea_data |> 
  mutate(
    longitude = as.numeric(longitude),
    latitude = as.numeric(latitude)
  )

# First summarize then round the results
physics_sea_data_reduced <- physics_sea_data |>
  group_by(longitude = round(longitude, 0),
           latitude = round(latitude, 0),
           time) |>
  summarize(
    thetao = mean(thetao, na.rm = TRUE),
    uo = mean(uo, na.rm = TRUE),
    vo = mean(vo, na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(
    thetao = round(thetao, 1),
    uo = round(uo, 2),
    vo = round(vo, 2)
  )

physics_data <- full_join(physics_sea_data_reduced, physics_ice_data_reduced,
                           by = c("longitude", "latitude", "time"))

physics_data <- physics_data |>
  rename(
    sea_water_potential_temperature = thetao,        # Sea water potential temperature [°C]
    eastward_sea_water_velocity = uo,                # Eastward sea water velocity [m/s]
    northward_sea_water_velocity = vo,               # Northward sea water velocity [m/s]
    sea_ice_area_fraction = siconc,                  # Sea ice area fraction
    sea_ice_thickness = sithick,                     # Sea ice thickness [m]
    sea_surface_height_above_sea_level = sla         # Sea surface height above sea level [m]
  )

# Save the data in RDS format
write_rds(combined_data, "combined_data.rds")

combined_data <- read_rds("combined_data.rds")

# New more detailed datasets -----------------------------------------------

nc_data_waves <- tidync("data_stream-wave.nc")

waves_data <- nc_data_waves |>
  hyper_filter() |>
  hyper_tibble() |>
  select(mwd, mwp, swh, p140209, hmax, tauoc, pp1d, shww, longitude, latitude, valid_time) |>
  mutate(valid_time = as.POSIXct(valid_time, origin = "1970-01-01", tz = "UTC"))

# Convert longitude and latitude to numeric
waves_data <- waves_data |> 
  mutate(
    longitude = as.numeric(longitude),
    latitude = as.numeric(latitude)
  )

# Round the results
waves_data <- waves_data |>
  mutate(
    mwd = round(mwd, 2),
    mwp = round(mwp, 2),
    swh = round(swh, 2),
    p140209 = round(p140209, 2),
    hmax = round(hmax, 2),
    tauoc = round(tauoc, 2),
    pp1d = round(pp1d, 2),
    shww = round(shww, 2)
)

waves_data <- waves_data |>
  rename(
    mean_wave_direction = mwd,
    mean_wave_period = mwp,
    significant_height_combined_waves_swell = swh,
    air_density_over_oceans = p140209,
    max_individual_wave_height = hmax,
    normalized_stress_into_ocean = tauoc,
    peak_wave_period = pp1d,
    significant_height_of_wind_waves = shww
)

#Another dataset
nc_data_other <- tidync("data_stream-oper.nc")

other_data <- nc_data_other |>
  hyper_filter() |>
  hyper_tibble() |>
  select(u10, v10, msl, sst, sp, tp, u10n, v10n, fg10, i10fg, msr, mtpr, cbh, 
         lcc, tcc, tciw, tclw, cp, crr, lsrr, lsp, ptype, tcrw, sf, cape, 
         siconc, tcwv, longitude, latitude, valid_time) |>
  mutate(valid_time = as.POSIXct(valid_time, origin = "1970-01-01", tz = "UTC"))

other_data_adjusted <- other_data |>
  mutate(
    longitude = as.numeric(longitude),
    latitude = as.numeric(latitude),
    longitude = round(longitude * 2) / 2,
    latitude = round(latitude * 2) / 2
)

other_data_cleaned <- other_data_adjusted |>
  distinct(longitude, latitude, valid_time, .keep_all = TRUE)

joined_data <- waves_data |>
  left_join(other_data_cleaned, by = c("longitude", "latitude", "valid_time"))

joined_data <- joined_data |>
  rename(
    u10_wind_component = u10,
    v10_wind_component = v10,
    mean_sea_level_pressure = msl,
    sea_surface_temperature = sst,
    surface_pressure = sp,
    total_precipitation = tp,
    u10_neutral_wind_component = u10n,
    v10_neutral_wind_component = v10n,
    max_wind_gust = fg10,
    instantaneous_wind_gust = i10fg,
    mean_snowfall_rate = msr,
    mean_total_precipitation_rate = mtpr,
    cloud_base_height = cbh,
    low_cloud_cover = lcc,
    total_cloud_cover = tcc,
    total_column_cloud_ice_water = tciw,
    total_column_cloud_liquid_water = tclw,
    convective_precipitation = cp,
    convective_rain_rate = crr,
    large_scale_rain_rate = lsrr,
    large_scale_precipitation = lsp,
    precipitation_type = ptype,
    total_column_rain_water = tcrw,
    snowfall = sf,
    convective_available_potential_energy = cape,
    sea_ice_concentration = siconc,
    total_column_water_vapour = tcwv,
    time = valid_time
)

joined_data <- joined_data |>
  mutate(
    u10_wind_component = round(u10_wind_component, 2),
    v10_wind_component = round(v10_wind_component, 2),
    mean_sea_level_pressure = round(mean_sea_level_pressure, 2),
    sea_surface_temperature = round(sea_surface_temperature - 273.15, 2),
    surface_pressure = round(surface_pressure, 2),
    total_precipitation = round(total_precipitation, 2),
    u10_neutral_wind_component = round(u10_neutral_wind_component, 2),
    v10_neutral_wind_component = round(v10_neutral_wind_component, 2),
    max_wind_gust = round(max_wind_gust, 2),
    instantaneous_wind_gust = round(instantaneous_wind_gust, 2),
    mean_snowfall_rate = round(mean_snowfall_rate, 2),
    mean_total_precipitation_rate = round(mean_total_precipitation_rate, 2),
    cloud_base_height = round(cloud_base_height, 2),
    low_cloud_cover = round(low_cloud_cover, 2),
    total_cloud_cover = round(total_cloud_cover, 2),
    total_column_cloud_ice_water = round(total_column_cloud_ice_water, 2),
    total_column_cloud_liquid_water = round(total_column_cloud_liquid_water, 2),
    convective_precipitation = round(convective_precipitation, 2),
    convective_rain_rate = round(convective_rain_rate, 2),
    large_scale_rain_rate = round(large_scale_rain_rate, 2),
    large_scale_precipitation = round(large_scale_precipitation, 2),
    precipitation_type = round(precipitation_type, 2),
    total_column_rain_water = round(total_column_rain_water, 2),
    snowfall = round(snowfall, 2),
    convective_available_potential_energy = round(convective_available_potential_energy, 2),
    sea_ice_concentration = round(sea_ice_concentration, 2),
    total_column_water_vapour = round(total_column_water_vapour, 2)
)

baltic_data <- baltic_data |>
  mutate(
    precipitation_type = case_when(
      precipitation_type == 0 ~ "No precipitation",
      precipitation_type == 1 ~ "Rain",
      precipitation_type == 3 ~ "Freezing rain",
      precipitation_type == 5 ~ "Snow",
      precipitation_type == 6 ~ "Wet snow",
      precipitation_type == 7 ~ "Rain and snow mix",
      precipitation_type == 8 ~ "Ice pellets",
      TRUE ~ "Unknown"
  )
)

# Save the data in RDS format
write_rds(baltic_data, "joined_data.rds")

baltic_data <- read_rds("joined_data.rds")

# Adding new variables based on existing ones ------------------------

baltic_data_wind_speed <- baltic_data |>
  mutate(
    wind_speed = sqrt(u10_wind_component^2 + v10_wind_component^2),
    .before = 1
  )

# Function to calculate wind direction
windDir <- function(u, v) {
  (270 - atan2(v, u) * 180 / pi) %% 360
}

baltic_data_wind_dir <- baltic_data_plus_wind_speed |>
  mutate(wind_direction = windDir(u10_wind_component, v10_wind_component))

baltic_data <- baltic_data_full |>
  mutate(wind_gust_factor = max_wind_gust / wind_speed,
         .before = 1
  )

# Add Beaufort category using case_when
baltic_data <- baltic_data |>
  mutate(beaufort_category = case_when(
    wind_speed < 0.3 ~ "0 (Calm)",
    wind_speed >= 0.3 & wind_speed < 1.5 ~ "1 (Light Air)",
    wind_speed >= 1.5 & wind_speed < 3.3 ~ "2 (Light Breeze)",
    wind_speed >= 3.3 & wind_speed < 5.5 ~ "3 (Gentle Breeze)",
    wind_speed >= 5.5 & wind_speed < 8.0 ~ "4 (Moderate Breeze)",
    wind_speed >= 8.0 & wind_speed < 10.8 ~ "5 (Fresh Breeze)",
    wind_speed >= 10.8 & wind_speed < 13.9 ~ "6 (Strong Breeze)",
    wind_speed >= 13.9 & wind_speed < 17.2 ~ "7 (Near Gale)",
    wind_speed >= 17.2 & wind_speed < 20.7 ~ "8 (Fresh Gale)",
    wind_speed >= 20.7 & wind_speed < 24.5 ~ "9 (Strong Gale)",
    wind_speed >= 24.5 & wind_speed < 28.4 ~ "10 (Storm)",
    wind_speed >= 28.4 & wind_speed < 32.6 ~ "11 (Violent Storm)",
    wind_speed >= 32.6 ~ "12 (Hurricane)"
  ))

baltic_data <- baltic_data |>
  mutate(douglas_category = case_when(
    significant_height_combined_waves_swell == 0 ~ "0 Calm (glassy)",
    significant_height_combined_waves_swell > 0 & significant_height_combined_waves_swell <= 0.1 ~ "1 Calm (rippled)",
    significant_height_combined_waves_swell > 0.1 & significant_height_combined_waves_swell <= 0.5 ~ "2 Smooth (wavelets)",
    significant_height_combined_waves_swell > 0.5 & significant_height_combined_waves_swell <= 1.25 ~ "3 Slight",
    significant_height_combined_waves_swell > 1.25 & significant_height_combined_waves_swell <= 2.5 ~ "4 Moderate",
    significant_height_combined_waves_swell > 2.5 & significant_height_combined_waves_swell <= 4 ~ "5 Rough",
    significant_height_combined_waves_swell > 4 & significant_height_combined_waves_swell <= 6 ~ "6 Very rough",
    significant_height_combined_waves_swell > 6 & significant_height_combined_waves_swell <= 9 ~ "7 High",
    significant_height_combined_waves_swell > 9 & significant_height_combined_waves_swell <= 14 ~ "8 Very high",
    significant_height_combined_waves_swell > 14 ~ "9 Phenomenal"
  ))

baltic_data <- baltic_data |>
  mutate(oktas_category = case_when(
    total_cloud_cover == 0 ~ "0 oktas (Sky clear)",
    total_cloud_cover > 0 & total_cloud_cover <= 0.125 ~ "1 okta (Fine)",
    total_cloud_cover > 0.125 & total_cloud_cover <= 0.25 ~ "2 oktas (Fine)",
    total_cloud_cover > 0.25 & total_cloud_cover <= 0.375 ~ "3 oktas (Partly Cloudy)",
    total_cloud_cover > 0.375 & total_cloud_cover <= 0.5 ~ "4 oktas (Partly Cloudy)",
    total_cloud_cover > 0.5 & total_cloud_cover <= 0.625 ~ "5 oktas (Partly Cloudy)",
    total_cloud_cover > 0.625 & total_cloud_cover <= 0.75 ~ "6 oktas (Cloudy)",
    total_cloud_cover > 0.75 & total_cloud_cover <= 0.875 ~ "7 oktas (Cloudy)",
    total_cloud_cover > 0.875 & total_cloud_cover <= 1 ~ "8 oktas (Overcast)"
  ))

# Define ordered factors for Beaufort and Douglas scales
baltic_data <- baltic_data |>
  mutate(
    beaufort_category = factor(beaufort_category, levels = c(
      "0 (Calm)", "1 (Light Air)", "2 (Light Breeze)", "3 (Gentle Breeze)",
      "4 (Moderate Breeze)", "5 (Fresh Breeze)", "6 (Strong Breeze)",
      "7 (Near Gale)", "8 (Fresh Gale)", "9 (Strong Gale)", 
      "10 (Storm)", "11 (Violent Storm)", "12 (Hurricane)"
    ), ordered = TRUE),
    douglas_category = factor(douglas_category, levels = c(
      "0 Calm (glassy)", "1 Calm (rippled)", "2 Smooth (wavelets)", 
      "3 Slight", "4 Moderate", "5 Rough", "6 Very rough", 
      "7 High", "8 Very high", "9 Phenomenal"
    ), ordered = TRUE)
  )

# New indexes
nc_data_indexes <- tidync("k_index_total_totals_index.nc")

indexes_data <- nc_data_indexes |>
  hyper_filter() |>
  hyper_tibble() |>
  select(kx, totalx, longitude, latitude, valid_time) |>
  mutate(valid_time = as.POSIXct(valid_time, origin = "1970-01-01", tz = "UTC"))

indexes_data <- indexes_data |>
  mutate(
    longitude = as.numeric(longitude),
    latitude = as.numeric(latitude),
    longitude = round(longitude * 2) / 2,
    latitude = round(latitude * 2) / 2
  )

indexes_data <- indexes_data |>
  distinct(longitude, latitude, valid_time, .keep_all = TRUE)

indexes_data <- indexes_data |>
  rename(time = valid_time)

baltic_data_with_indexes <- baltic_data |>
  left_join(indexes_data, by = c("longitude", "latitude", "time"))

baltic_data_with_indexes <- baltic_data_with_indexes |>
  rename(k_index = kx, total_totals_index = totalx)

# Save the data in RDS format
baltic_data <- read_rds("baltic_data.rds")

# Calculate wavelength (L)
baltic_data_normalized <- baltic_data_normalized |>
  mutate(
    wavelength = (9.8 * mean_wave_period^2) / (2 * pi)
  )

# Calculate wave steepness
baltic_data_normalized <- baltic_data_normalized |>
  mutate(
    wave_steepness = significant_height_combined_waves_swell / wavelength
  )

# Calculate wind stress using air_density_over_oceans
baltic_data_normalized <- baltic_data_normalized |>
  mutate(
    wind_stress = air_density_over_oceans * 0.0013 * wind_speed^2
  )

#Adding 3 new categorical variables to baltic_data for plotting
baltic_data <- baltic_data |>
  mutate(
    # Create season variable
    season = case_when(
      month(time) %in% c(12, 1, 2) ~ "Winter",
      month(time) %in% c(3, 4, 5) ~ "Spring",
      month(time) %in% c(6, 7, 8) ~ "Summer",
      month(time) %in% c(9, 10, 11) ~ "Fall"
    ),
    # Categorize total_totals_index
    total_index_category = case_when(
      total_totals_index < 44 ~ "No thunderstorms",
      total_totals_index >= 44 & total_totals_index <= 50 ~ "Thunderstorms likely",
      total_totals_index >= 51 & total_totals_index <= 52 ~ "Isolated severe thunderstorms",
      total_totals_index >= 53 & total_totals_index <= 56 ~ "Widely scattered severe thunderstorms",
      total_totals_index > 56 ~ "Scattered severe thunderstorms likely"
    ),
    # Categorize k_index
    k_index_category = case_when(
      k_index < 20 ~ "No thunderstorms",
      k_index >= 20 & k_index <= 25 ~ "Isolated thunderstorms",
      k_index >= 26 & k_index <= 30 ~ "Widely scattered thunderstorms",
      k_index >= 31 & k_index <= 35 ~ "Scattered thunderstorms",
      k_index > 35 ~ "Numerous thunderstorms"
    )
  )

#Convert it to an ordered factor
baltic_data$total_index_category <- factor(
  baltic_data$total_index_category,
  levels = c(
    "No thunderstorms",
    "Thunderstorms likely",
    "Isolated severe thunderstorms",
    "Widely scattered severe thunderstorms",
    "Scattered severe thunderstorms likely"
  ),
  ordered = TRUE
)

baltic_data$k_index_category <- factor(
  baltic_data$k_index_category,
  levels = c(
    "No thunderstorms",
    "Isolated thunderstorms",
    "Widely scattered thunderstorms",
    "Scattered thunderstorms",
    "Numerous thunderstorms"
  ),
  ordered = TRUE
)

# Assigning labels --------------------------------------------------------

baltic_data_labeled <- baltic_data |>
  mutate(
    safety_label = case_when(
      # Unsafe conditions
      wind_speed >= 10.8 ~ "Unsafe",
      significant_height_combined_waves_swell >= 3 ~ "Unsafe",
      
      # Risky conditions
      sea_ice_concentration > 0.3 ~ "Risky",
      wind_speed >= 8.0 & wind_speed < 10.8 ~ "Risky",
      
      # Safe conditions (default)
      TRUE ~ "Safe"
    )
  )

# Count the number of observations in each safety_label category
safety_label_counts <- baltic_data_labeled |>
  count(safety_label)

print(safety_label_counts)

baltic_data_labeled <- baltic_data_labeled |>
  mutate(
    safety_label = factor(
      safety_label,
      levels = c("Safe", "Risky", "Unsafe"), # Order the levels logically
      ordered = TRUE
    )
  )

baltic_data_labeled <- read_rds("baltic_data_labeled.rds")

# Cleaning the labeled dataset for modeling ---------------------------------------

baltic_data_model <- baltic_data_labeled |>
  select(-c(total_column_cloud_liquid_water, wind_stress, 
            total_column_cloud_ice_water, normalized_stress_into_ocean,
            air_density_over_oceans, total_column_rain_water, wave_steepness,
            total_precipitation, convective_precipitation, large_scale_precipitation,
            mean_snowfall_rate, mean_total_precipitation_rate, convective_rain_rate,
            large_scale_rain_rate, snowfall))

baltic_data_model <- baltic_data_model |>
  select(-c(u10_wind_component, v10_wind_component, 
            u10_neutral_wind_component, v10_neutral_wind_component,
            cloud_base_height, total_column_water_vapour))

write_rds(baltic_data_model, "baltic_data_model.rds")

baltic_data_labeled <- read_rds("baltic_data_labeled.rds")

baltic_data <- read_rds("baltic_data.rds")

baltic_data_normalized <- read_rds("baltic_data_normalized.rds")

baltic_data_model <- read_rds("baltic_data_model.rds")

# Plots -------------------------------------------------------------------
library(openair)
library(paletteer)
library(ggthemes)
library(maps)
library(sf)
library(patchwork)

# Use in a ggplot2 chart:
scale_colour_paletteer_d("ggsci::light_blue_material")
scale_fill_paletteer_d("ggsci::light_blue_material")

scale_color_paletteer_c("ggthemes::Blue-Teal")
scale_fill_paletteer_c("ggthemes::Blue-Teal")

baltic_data_labeled |>
  ggplot(aes(x = beaufort_category, y = wind_speed, fill = safety_label)) +  # Change y to wind_speed if no safety score exists
  geom_boxplot(outlier.shape = 16, outlier.size = 2, alpha = 0.7) +
  labs(
    title = "Box Plot of Wind Speed by Beaufort Categories and Safety Labels",
    x = "Beaufort Category",
    y = "Wind Speed [m/s]"
  ) +
  scale_fill_paletteer_d("ggsci::light_blue_material", name = "Safety Level") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

baltic_data_labeled |>
  ggplot(aes(x = wind_speed, fill = safety_label)) +  # Change x to wind_speed or another variable if no safety_score exists
  geom_histogram(binwidth = 1, alpha = 0.7, position = "identity") +
  labs(
    title = "Distribution of Wind Speed by Safety Labels",
    x = "Wind Speed [m/s]",
    y = "Frequency"
  ) +
  scale_fill_paletteer_d("ggsci::light_blue_material", name = "Safety Level") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5)
  )

baltic_data_labeled |>
  ggplot(aes(x = beaufort_category, y = wind_speed, color = safety_label)) +  # wind_speed or appropriate variable
  geom_boxplot(alpha = 0.5) +
  geom_jitter(width = 0.2, size = 2, alpha = 0.7) +
  scale_colour_paletteer_d("ggsci::light_blue_material", name = "Safety Level") +
  labs(
    title = "Wind speed by Beaufort category and safety labels",
    x = "Beaufort category",
    y = "Wind speed [m/s]",
    color = "Safety level"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

windRose(baltic_data, ws = "wind_speed", wd = "wind_direction")

ggplot(data = baltic_data,
       mapping = aes(x = wind_speed, y = max_individual_wave_height, 
                     color = oktas_category)
) +
  geom_point(size = 0.8) +
  scale_colour_paletteer_d("ggsci::light_blue_material") +
  labs(
    x = "Wind speed [m/s]",
    y = "Maximum individual wave height [m]",
    title = "Wind speed compared to max. individual wave height by Oktas category",
    color = "Oktas scale"
  ) +
  theme_gray()

ggplot(data = baltic_data, mapping = aes(x = time, y = significant_height_combined_waves_swell)) +
  geom_point(alpha = 0.01) +
  geom_smooth()  +
  labs(
    x = "Time",
    y = "Significant height of combined waves and swell [m]",
    title = "Trend of significant wave height over time"
  ) +
  theme_gray()

ggplot(data = baltic_data, mapping = aes(x = mean_wave_period)) +
  geom_density(fill = "skyblue", alpha = 0.7) +
  labs(
    x = "Mean wave period [s]",
    y = "Density",
    title = "Density distribution of mean wave period",
    subtitle = "A visualization of the distribution of mean wave periods"
  ) +
  theme_gray()

# Hexbin plot with continuous sequential palette
ggplot(data = baltic_data, 
       mapping = aes(x = significant_height_combined_waves_swell, 
                     y = max_wind_gust)) +
  geom_hex(bins = 50) +
  scale_fill_paletteer_c("ggthemes::Blue-Teal") +
  labs(
    x = "Significant height of combined waves and swell [m]",
    y = "Maximum wind gust [m/s]",
    title = "Wave height vs. wind gust",
    subtitle = "Hexbin plot for better performance with large datasets",
    fill = "Density"
  ) +
  theme_gray()

ggplot(data = baltic_data, 
       mapping = aes(x = time, y = sea_surface_temperature)) +
  geom_smooth() +
  labs(
    x = "Time",
    y = "Sea surface temperature [°C]",
    title = "Sea surface temperature over time"
  ) +
  theme_gray()

ggplot(data = baltic_data, 
       mapping = aes(x = precipitation_type, y = sea_surface_temperature)) +
  geom_boxplot() +
  labs(
    x = "Precipitation type",
    y = "Sea surface temperature [°C]",
    title = "Distribution of sea surface temperature by precipitation type"
  ) +
  theme_gray()

# Air Density vs. Sea Surface Temperature
ggplot(data = baltic_data, 
       mapping = aes(x = air_density_over_oceans, y = sea_surface_temperature)) +
  geom_hex(bins = 30) +
  scale_fill_paletteer_c("ggthemes::Blue-Teal") +
  labs(
    x = expression("Air density over oceans [kg/m"^3*"]"),
    y = "Sea surface temperature [°C]",
    title = "Air density vs. sea surface temperature"
  ) +
  theme_gray()

ggplot(data = baltic_data, 
       mapping = aes(x = low_cloud_cover, y = significant_height_combined_waves_swell)
) +
  geom_point(alpha = 0.01, color = "blue") +
  labs(
    x = "Low cloud cover [fraction]",
    y = "Significant height of combined waves and swell [m]",
    title = "Low cloud cover vs. wave height",
    subtitle = "Exploring the relationship between cloud cover and wave conditions",
    color = "Wave height"
  ) +
  theme_gray()

ggplot(baltic_data, aes(x = wind_speed)) +
  geom_histogram(bins = 30, alpha = 0.8) +
  labs(x = "Wind speed [m/s]", y = "Frequency", title = "Distribution of wind speed") +
  theme_gray()

ggplot(baltic_data, aes(x = douglas_category, y = mean_wave_period, colour = douglas_category)) +
  geom_boxplot() +
  scale_colour_paletteer_d("ggsci::light_blue_material") +
  labs(
    x = "Douglas category",
    y = "Mean wave period [s]",
    title = "Distribution of mean wave period by Douglas category",
    color = "Douglas scale"
  ) +
  theme_gray() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1), 
    axis.text = element_text(size = 10) 
  )

baltic_data |>
  count(beaufort_category, douglas_category) |>
  ggplot(aes(x = beaufort_category, y = douglas_category)) +
  geom_tile(aes(fill = n)) +
  scale_fill_paletteer_c("ggthemes::Blue-Teal", name = "Count") + 
  labs(
    x = "Beaufort category",
    y = "Douglas category",
    title = "Heatmap of Beaufort and Douglas categories"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1), 
    axis.text = element_text(size = 10) 
  )

baltic_data |>
  count(beaufort_category, oktas_category) |>
  ggplot(aes(x = beaufort_category, y = oktas_category)) +
  geom_tile(aes(fill = n)) +
  scale_fill_paletteer_c("ggthemes::Blue-Teal", name = "Count") + 
  labs(
    x = "Beaufort category",
    y = "Oktas category",
    title = "Heatmap of Beaufort and Oktas categories"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1), 
    axis.text = element_text(size = 10) 
  )

baltic_data |>
  count(douglas_category, oktas_category) |>
  ggplot(aes(x = douglas_category, y = oktas_category)) +
  geom_tile(aes(fill = n)) +
  scale_fill_paletteer_c("ggthemes::Blue-Teal", name = "Count") + 
  labs(
    x = "Douglas category",
    y = "Oktas category",
    title = "Heatmap of Douglas and Oktas categories"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1), 
    axis.text = element_text(size = 10) 
  )

ggplot(data = baltic_data,
       mapping = aes(x = wavelength, y = wind_speed)
) + 
  geom_point(alpha = 1/100) +
  labs(
    x = "Wavelength [m]", y = "Wind speed [m/s]",
    title = "Wavelength compared to wind speed"
  ) +
  theme_gray()

ggplot(baltic_data, aes(x = surface_pressure, y = instantaneous_wind_gust)) +
  geom_point(aes(color = beaufort_category)) +
  geom_smooth(se = FALSE) +
  scale_colour_paletteer_d("ggsci::light_blue_material") +
  labs(
    x = "Surface pressure [Pa]",
    y = "Instantaneous wind gust [m/s]",
    color = "Beaufort category",
    title = "Relationship between surface pressure and wind gusts",
    subtitle = "Surface pressure at sea level compared to wind gust intensity at 10m height",
    caption = "Source: ERA5 reanalysis data - Copernicus Climate Data Store (CDS)"
  ) +
  theme_gray()

# First plot: total_column_water_vapour vs max_individual_wave_height
plot1 <- ggplot(baltic_data, aes(x = total_column_water_vapour, y = max_individual_wave_height)) +
  geom_point(alpha = 0.7) +
  geom_smooth(se = FALSE) +
  labs(
    x = "Total column water vapour [kg/m²]",
    y = "Maximum individual wave height [m]",
  )

# Second plot: air_density_over_oceans vs max_individual_wave_height
plot2 <- ggplot(baltic_data, aes(x = air_density_over_oceans, y = max_individual_wave_height)) +
  geom_point(alpha = 0.7) +
  geom_smooth(se = FALSE) +
  labs(
    x = "Air density over oceans [kg/m³]",
    y = "Maximum individual wave height [m]",
  )

# Combine guide area, plots, and annotations
(guide_area() / (plot1 + plot2)) +
  plot_annotation(
    title = "Wave height relationships with atmospheric conditions",
    subtitle = "Examining how water vapour and air density relate to maximum wave height",
    caption = "Source: ERA5 reanalysis data - Copernicus Climate Data Store (CDS)"
  ) +
  theme_gray() +
  plot_layout(
    guides = "collect",
    heights = c(1, 4)  # Adjusted proportions
  ) +
  theme(
    legend.position = "top",
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5),
    plot.caption = element_text(size = 8, hjust = 1)
  )

# Time-series plots -------------------------------------------------------
library(TSstudio)

# Use in a ggplot2 chart:
scale_colour_paletteer_d("ggsci::light_blue_material")
scale_fill_paletteer_d("ggsci::light_blue_material")

scale_color_paletteer_c("ggthemes::Blue-Teal")
scale_fill_paletteer_c("ggthemes::Blue-Teal")

# Filter data for a specific location
middle_baltic_data <- baltic_data |>
  filter(latitude == 58.5, longitude == 20.5)

# Verify the filtered data
glimpse(middle_baltic_data)

# Create a daily time series
sea_temp_ts <- ts(
  middle_baltic_data$sea_surface_temperature,
  start = c(2012, 1), # Adjust to the starting year and period
  frequency = 365.25 * 3 # Adjusted frequency for leap + standard years
)

max_wave_height_ts <- ts(
  middle_baltic_data$max_individual_wave_height,
  start = c(2012, 1),
  frequency = 365.25 * 3
)

# Check the time series object
print(sea_temp_ts)

# Perform and visualize decomposition
ts_decompose(sea_temp_ts)
ts_decompose(max_wave_height_ts)

ggplot(data = baltic_data,
       mapping = aes(x = time, y = max_individual_wave_height)
) +
  geom_smooth() +
  labs(
    x = "Time", y = "Max. individual wave height [m]",
    title = "It appears that maximum wave height varies seasonally",
    subtitle = "It is calm especially in the summer"
  ) +
  theme_gray()

# Most basic bubble plot
ggplot(data = baltic_data, aes(x = as.Date(time), y = wind_speed)) +
  geom_line(color = "skyblue") + 
  geom_point(alpha = 1/100) + 
  scale_x_date(
    limits = as.Date(c("2012-01-01", "2012-12-31")),
    date_breaks = "1 month",
    date_labels = "%b"
  ) +
  labs(
    title = "Daily wind speed",
    x = "Time (2012)",
    y = "Wind speed [m/s]"
  ) +
  theme_gray() +
  theme(
    axis.text.x = element_text(angle = 60, hjust = 1)
  )

# Map plots ---------------------------------------------------------------

# Hexbin map summarizing convective_available_potential_energy
baltic_data |>
  ggplot(aes(x = longitude, y = latitude)) +
  stat_summary_hex(
    aes(z = convective_available_potential_energy),  # Variable to summarize
    fun = mean,  # Summary statistic
    bins = 20
  ) +
  scale_fill_paletteer_c("ggthemes::Blue-Teal", name = "Mean CAPE [J/kg]") +
  coord_quickmap() +  # Real-world aspect ratio
  labs(
    x = "Longitude",
    y = "Latitude",
    title = "Mean Convective Available Potential Energy in the Baltic Sea"
  ) +
  theme_minimal()

baltic_map <- map_data("world") |>
  filter(lat > 53, lat < 66, long > 9, long < 31)

ggplot() +
  geom_polygon(data = baltic_map, aes(x = long, y = lat, group = group),
               fill = "gray80", color = "black") +
  stat_summary_hex(
    data = baltic_data,
    aes(x = longitude, y = latitude, z = convective_available_potential_energy), 
    fun = mean, 
    bins = 20
  ) +
  scale_fill_paletteer_c("ggthemes::Blue-Teal", name = "Mean CAPE [J/kg]") +
  coord_quickmap() +
  labs(
    x = "Longitude",
    y = "Latitude",
    title = "Mean Convective Available Potential Energy in the Baltic Sea"
  ) +
  theme_minimal()

baltic_data |>
  ggplot(aes(x = longitude, y = latitude)) +
  stat_summary_hex(
    aes(z = significant_height_combined_waves_swell),
    fun = max,  # Use max instead of mean
    bins = 20
  ) +
  scale_fill_paletteer_c("ggthemes::Blue-Teal", name = "Max wave and swell height [m]") +
  coord_quickmap() +  # Real-world aspect ratio
  labs(
    x = "Longitude",
    y = "Latitude",
    title = "Maximum significant wave and swell height in the Baltic Sea"
  ) +
  theme_minimal()


# Heatmap and frequency table ---------------------------------------------
library(tidyquant)
library(plotly)
library(tidyclust)
library(tidymodels)

baltic_ts <- baltic_data_normalized |>
  as_tsibble(index = time)

baltic_ts <- baltic_data_normalized |>
  as_tsibble(index = time)

baltic_data_scales <- baltic_data_normalized |>
  select(beaufort_category, douglas_category)

table(baltic_data_scales)

# Feature selection -------------------------------------------------------
library(caret)
library(reshape2)

# Calculate variance for numeric variables
baltic_data_variances <- baltic_data |>
  summarize(across(where(is.numeric), \(x) var(x, na.rm = TRUE)))

# Sort the variances for easier interpretation
baltic_data_variances <- baltic_data_variances |>
  pivot_longer(cols = everything(), 
               names_to = "Feature", 
               values_to = "Variance",
               values_drop_na = TRUE) |>
  arrange(desc(Variance))

options(scipen = 999)

# Define a threshold
variance_threshold <- 0.01

# Filter out features below the threshold
selected_features <- baltic_data_variances |>
  filter(Variance >= variance_threshold)

# Extract the feature names from `selected_features`
selected_feature_names <- selected_features$Feature

# Modeling ----------------------------------------------------------------
library(forecast)
library(TTR)
library(geosphere)
library(sp)
library(zoo)
library(paletteer)
library(lubridate)

baltic_data_model <- read_rds("../baltic_data_model.rds")

# Define start and end points
start <- c(18.0, 55.0)  # Longitude, Latitude
end <- c(19.0, 56.0)    # Longitude, Latitude

# Generate intermediate points
route <- gcIntermediate(start, end, n = 50, addStartEnd = TRUE, sp = TRUE)

# Convert to a data frame for visualization
route_df <- as.data.frame(coordinates(route))
colnames(route_df) <- c("longitude", "latitude")

# Plot the route
ggplot(route_df, aes(x = longitude, y = latitude)) +
  geom_path() +
  geom_point() +
  labs(title = "Route from Start to End", x = "Longitude", y = "Latitude") +
  theme_minimal()

# Filter data for 2012
training_data <- baltic_data_model |>
  filter(lubridate::year(time) == 2012)

# Group by latitude and longitude
grouped_data <- training_data |>
  group_by(latitude, longitude) |>
  group_split()

# Initialize list to store results
results <- list()

# Define WMA window size
window_size <- 3  # One day's worth of observations (06:00, 12:00, 18:00)

# Loop through each spatial group
for (i in seq_along(grouped_data)) {
  loc_data <- grouped_data[[i]]
  
  # Prepare wind_speed as a numeric vector
  ts_data <- loc_data |>
    arrange(time) |>
    pull(wind_speed)
  
  # Calculate Weighted Moving Average (WMA)
  wma_result <- TTR::WMA(ts_data, n = window_size)
  
  # Generate forecasts dynamically for January 2024
  forecast_values <- numeric(31 * 3)  # Placeholder for 93 predictions
  for (j in seq_along(forecast_values)) {
    recent_values <- c(ts_data, forecast_values[1:(j - 1)])
    wma_forecast <- TTR::WMA(recent_values, n = window_size)
    forecast_values[j] <- tail(wma_forecast, 1)
  }
  
  # Generate future timestamps for January 2024
  future_dates <- seq.Date(
    from = as.Date("2024-01-01"),
    to = as.Date("2024-01-31"),
    by = "day"
  )
  
  # Combine dates with specific times
  future_times <- as.POSIXct(paste(rep(future_dates, each = 3),
                                   rep(c("06:00:00", "12:00:00", "18:00:00"), times = length(future_dates))))
  
  # Store results
  results[[i]] <- tibble(
    latitude = unique(loc_data$latitude),
    longitude = unique(loc_data$longitude),
    time = future_times,
    predicted_wind_speed = forecast_values
  )
}

# Combine all results into a single data frame
forecast_results <- bind_rows(results)

# Visualization for a specific timestamp
forecast_results |>
  filter(time == as.POSIXct("2024-01-01 06:00:00")) |>
  ggplot(aes(x = longitude, y = latitude, fill = predicted_wind_speed)) +
  geom_tile() +
  scale_fill_paletteer_c("ggthemes::Blue-Teal") +
  labs(
    title = "Predicted Wind Speeds Across the Baltic Sea on 2024-01-01 06:00",
    x = "Longitude",
    y = "Latitude",
    fill = "Wind Speed"
  ) +
  theme_minimal()

# All variables

# Filter data for 2012
training_data <- baltic_data_model |>
  filter(year(time) == 2012)

# Identify numeric variables to forecast
numeric_vars <- names(training_data)[sapply(training_data, is.numeric)]
numeric_vars <- setdiff(numeric_vars, c("latitude", "longitude"))

# Group by latitude and longitude
grouped_data <- training_data |>
  group_by(latitude, longitude) |>
  group_split()

# Define WMA window size
window_size <- 3  # for example, 3 observations per day (06:00, 12:00, 18:00)
# Define forecast horizon: 31 days * 3 obs/day = 93 predictions
future_length <- 31 * 3

# Generate future timestamps for January 2024
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

# Loop through each spatial group
for (i in seq_along(grouped_data)) {
  loc_data <- grouped_data[[i]] |>
    arrange(time)
  
  # Store forecast results for each numeric variable
  forecast_list <- vector("list", length(numeric_vars))
  
  for (v in seq_along(numeric_vars)) {
    var_name <- numeric_vars[v]
    
    # Extract and interpolate time series for the current variable
    ts_data <- loc_data |>
      pull(var_name)
    
    # Interpolate missing values
    ts_data <- na.approx(ts_data, rule = 2)
    
    # Generate forecasts using a rolling WMA
    wma_forecasts <- numeric(future_length)
    for (j in seq_along(wma_forecasts)) {
      recent_values <- c(ts_data, wma_forecasts[1:(j - 1)])
      # Interpolate again in case any NAs appear after combining
      recent_values <- na.approx(recent_values, rule = 2)
      
      # Check length before applying WMA
      if (length(recent_values) < window_size) {
        # Not enough data points to compute WMA, assign NA
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

# Combine all results into a single data frame
forecast_results <- bind_rows(results)

# Example Visualization for a single variable at a specific timestamp
forecast_results |>
  filter(time == as.POSIXct("2013-01-01 06:00:00", tz = "UTC")) |>
  ggplot(aes(x = longitude, y = latitude, fill = predicted_wind_speed)) +
  geom_tile() +
  scale_fill_paletteer_c("ggthemes::Blue-Teal") +
  labs(
    title = "Predicted Wind Speeds Across the Baltic Sea on 2024-01-01 06:00",
    x = "Longitude",
    y = "Latitude",
    fill = "Wind Speed"
  ) +
  theme_minimal()

# Step 1: Filter Actual Data for January 2013
actual_data_jan_2013 <- baltic_data_model |>
  filter(year(time) == 2013, month(time) == 1) |>
  select(latitude, longitude, time, wind_speed)  # Focus on wind_speed

# Step 2: Filter Predicted Data for January 2013
forecast_results_jan_2013 <- forecast_results |>
  filter(year(time) == 2013, month(time) == 1) |>
  select(latitude, longitude, time, predicted_wind_speed)

forecast_results_jan_2013 <- forecast_results_jan_2013 |>
  arrange(latitude, longitude, time)

actual_data_jan_2013 <- actual_data_jan_2013 |>
  arrange(latitude, longitude, time)

# Step 3: Join Predicted and Actual Data
comparison <- forecast_results_jan_2013 |>
  inner_join(actual_data_jan_2013, by = c("latitude", "longitude", "time")) |>
  rename(predicted = predicted_wind_speed, actual = wind_speed)

MAE <- mean(abs(comparison$predicted - comparison$actual), na.rm = TRUE)
RMSE <- sqrt(mean((comparison$predicted - comparison$actual)^2, na.rm = TRUE))

# Print metrics
print(MAE)
print(RMSE)

ggplot(comparison, aes(x = actual, y = predicted)) +
  geom_point(alpha = 0.5, color = "blue") +
  geom_abline(slope = 1, intercept = 0, color = "red") +
  labs(
    title = "Predicted vs. Actual Wind Speeds (January 2013)",
    x = "Actual Wind Speed",
    y = "Predicted Wind Speed"
  ) +
  theme_minimal()


