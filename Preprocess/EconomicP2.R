library(sf)
library(readxl)
library(tidyverse)
library(osmdata)
library(rgeoboundaries)
library(leaflet)
library(tmap)

source('utils/fullpreprocess.R')
source('utils/boundaries.R')

# Annual Average Air Temperature (°C). Absolute values for 1981-2000 and 2001-2020
# and change values for a range of future Global Warming Levels,
# relative to the 1981-2000 baseline
# https://climate-themetoffice.hub.arcgis.com/datasets/TheMetOffice::annual-average-temperature-change-projections-local-authority-v1/about

temp <- st_read("./PData/Individual/TempWater/annual_average_temperature_change_projections_local_authority_v1_-8116657329075127237/annual_average_temperature_change_projections_local_authority_v1.shp")
temp_name <- read_csv("./PData/Individual/TempWater/annual_average_temperature_change_projections_local_authority_v1_-7277948155124131527.csv")
temp_name%>%colnames()
# col interpretation: e.g. "TAS Annual 2°C lower": If the global average temperature reaches 2°C,
# how much lower the annual average air temperature is projected to be in that area,
# relative to the 1981-2000 baseline.
cols <- c("TAS Annual 1981-2000 lower", "TAS Annual 1981-2000 median",
"TAS Annual 1981-2000 upper", "TAS Annual 2001-2020 lower", "TAS Annual 2001-2020 median",
"TAS Annual 2001-2020 upper",  "TAS Annual 1.5°C lower", "TAS Annual 1.5°C median",
"TAS Annual 1.5°C upper", "TAS Annual 2°C lower", "TAS Annual 2°C median",
"TAS Annual 2°C upper", "TAS Annual 2.5°C lower", "TAS Annual 2.5°C median",
"TAS Annual 2.5°C upper", "TAS Annual 3°C lower", "TAS Annual 3°C median",
"TAS Annual 3°C upper", "TAS Annual 3.5°C lower", "TAS Annual 3.5°C median",
"TAS Annual 3.5°C upper", "TAS Annual 4°C lower", "TAS Annual 4°C median",
"TAS Annual 4°C upper")
colnames(temp)[4:27] <- cols

# Wind Speed Data
# The dataset is derived from projections of seasonal mean wind speeds from UKCP18
# which are averaged to produce values for the 1981-2000 baseline and two warming levels:
# 2.0°C and 4.0°C above the pre-industrial (1850-1900) period. All wind speeds have units
# of metres per second (m / s). These data enable users to compare future seasonal mean wind
# speeds to those of the baseline period.
# https://climate-themetoffice.hub.arcgis.com/datasets/6b2bee0ed29749caaaf9c49f5ddd3a7f_0/explore?location=58.185175%2C-5.068438%2C7.53
file_path <- '/Users/wangqiqian/Desktop/UCL/SAG/PData/Individual/TempWater/Seasonal_Average_Wind_Speed_Projections_5km_-5913176971528534346.gpkg'
st_layers(file_path)
wind_bng <- st_read(file_path)
england_bng <- england_bng%>%
  st_as_sf()%>%
  st_transform(st_crs(wind_bng))
wind_bng <- st_intersection(wind_bng, england_bng)

for (i in c('summer', 'autumn', 'winter', 'spring')){
  choose <- paste('ws_', i, '_baseline_median', sep = '')
  suitability_points <- calculate_distance(
    wind_bng, grid_size=1000, type='area', save_name=paste('./Data/Tif/', choose, sep = ''),
    max_dist=5000, suitability_type='decrease', area_value = choose)
}

wind_bng <- wind_bng%>%
  mutate(
    ws_avg_baseline_median = (ws_summer_baseline_median +
                                ws_winter_baseline_median +
                                ws_autumn_baseline_median +
                                ws_spring_baseline_median)/4)

suitability_points <- calculate_distance(
  wind_bng, grid_size=1000, type='area', save_name='./Data/Tif/ws_avg_baseline_median',
  max_dist=5000, suitability_type='decrease', area_value = 'ws_avg_baseline_median')

# Drought
# The DSI 12 month accumulations are calculated for two baseline (historical) periods 1981-2000
# (corresponding to 0.51°C warming) and 2001-2020 (corresponding to 0.87°C warming)
# and for global warming levels of 1.5°C, 2.0°C, 2.5°C, 3.0°C, 4.0°C above the pre-industrial
# (1850-1900) period.
# https://climate-themetoffice.hub.arcgis.com/datasets/b9e6f84d2ee943d0be17d93366bca8dc_0/explore?location=55.082647%2C-3.273213%2C6.19
file_path <- './PData/Individual/TempWater/Drought_Severity_Index_12_Month_Accumulations_1364492214063738842.gpkg'
st_layers(file_path)
drought_data <- st_read(file_path)
england_bng <- england_bng%>%
  st_as_sf()%>%
  st_transform(st_crs(drought_data))
drought_data <- st_intersection(drought_data, england_bng)

# water
# https://environment.data.gov.uk/dataset/62514eb5-e9d5-4d96-8b73-a40c5b702d43
file_path <- './PData/Individual/TempWater/Water_Resource_Availability_and_Abstraction_Reliability_Cycle_2.gpkg'
st_layers(file_path)
# Q95 represent when the river is at its driest, lowest flow 5% of days in a year
# (e.g., hottest August), what is its flow. so green is good
# resavail: summary of camscdsq30, camscdsq50, camscdsq70, camscdsq95
water <- st_read(file_path, layer = 'Resource_Availability_at_Q95')
water_simple <- st_simplify(water, preserveTopology = TRUE, dTolerance = 100)
water_simple <- st_transform(water_simple, 4326)

choose <- 'resavail'
calculate_distance(
  water_simple, grid_size=1000, type='area', save_name=paste('./Data/Tif/', choose, sep = ''),
  max_dist=5000, suitability_type='decrease', area_value = 'resavail')
