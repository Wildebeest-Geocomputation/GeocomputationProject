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

temp <- temp%>%
  filter(!NAME %in% c('Scottish Borders', 'Dumfries and Galloway'))
temp_bng <- temp%>%
  st_transform(crs = "EPSG:27700")%>%
  st_filter(st_as_sf(england_bng))

tmap_mode("view")
tm_basemap("CartoDB.Positron") +
  tm_shape(temp_bng) +
  tm_polygons("TAS Annual 1981-2000 lower", palette = "YlOrRd")


source('Analysis/FullPreprocess.R')
suitability_points <- calculate_distance(temp_bng, grid_size=1000, type='area', save_name='./Data/Tif/temperature_change',
                                         max_dist=5000, suitability_type='decrease',
                                         area_value = "TAS Annual 2001-2020 median")

# Wind Speed Data
# https://climate-themetoffice.hub.arcgis.com/datasets/6b2bee0ed29749caaaf9c49f5ddd3a7f_0/explore?location=58.185175%2C-5.068438%2C7.53
file_path <- '/Users/wangqiqian/Desktop/UCL/SAG/PData/Individual/TempWater/Seasonal_Average_Wind_Speed_Projections_5km_-5913176971528534346.gpkg'
st_layers(file_path)
wind_data <- st_read(file_path)
tmap_mode("view")
tm_basemap("CartoDB.Positron") +
  tm_shape(wind_data) +
  tm_polygons("ws_spring_baseline_median", palette = "YlOrRd")

wind_bng <- wind_data%>%
  st_transform(crs = "EPSG:27700")%>%
  st_filter(st_as_sf(england_bng))

# summer autumn winter spring
choose <- 'ws_winter_baseline_median'
suitability_points <- calculate_distance(wind_bng, grid_size=1000, type='area', save_name=paste('./Data/Tif/', choose, sep = ''),
                                         max_dist=5000, suitability_type='decrease',
                                         area_value = choose)

# Drought
# https://climate-themetoffice.hub.arcgis.com/datasets/b9e6f84d2ee943d0be17d93366bca8dc_0/explore?location=55.082647%2C-3.273213%2C6.19
file_path <- '/Users/wangqiqian/Desktop/UCL/SAG/PData/Individual/TempWater/Drought_Severity_Index_12_Month_Accumulations_1364492214063738842.gpkg'
st_layers(file_path)
drought_data <- st_read(file_path)
tmap_mode("view")
tm_basemap("CartoDB.Positron") +
  tm_shape(drought_data) +
  tm_polygons("DSI12_baseline_00_17_median", palette = "YlOrRd")

choose <- 'DSI12_baseline_00_17_median'
suitability_points <- calculate_distance(drought_data, grid_size=1000, type='area',
                                         save_name=paste('./Data/Tif/', choose, sep = ''),
                                         max_dist=5000, suitability_type='decrease',
                                         area_value = choose)

