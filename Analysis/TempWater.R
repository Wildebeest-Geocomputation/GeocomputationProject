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

file_path <- './PData/Individual/TempWater/annual_average_temperature_change_projections_local_authority_v1_-8872211456291778675.gpkg'
st_layers(file_path)
temp <- st_read(file_path)
temp_bng <- temp %>% filter(str_starts(CODE, "E"))

tmap_mode("plot")
temp_map <- tm_basemap("CartoDB.Positron") +
  tm_shape(temp_bng) +
  tm_polygons(
    c('tas_annual_8100_median', 'tas_annual_0120_median'),
    title = 'DSI',
    palette = "YlOrRd",
    border.alpha = 0,
    style = "cont",
    breaks = c(0, 14)) +
  tm_facets(ncol = 2, free.scales = FALSE) +
  tm_layout(
    panel.labels = c("1981-2000", "2001-2020"),
    legend.outside = FALSE,
    legend.position = c("right", "top"),
    legend.bg.color = "white",
    legend.bg.alpha = 0.6,
    legend.frame = TRUE,
    main.title = 'Median Air Temperature (°C)')

tmap_save(temp_map, filename = "Data/Layout/map air temperature.png",
          width = 10, height = 8, units = "in", dpi = 300,
          device = ragg::agg_png)

# for (i in c('tas_annual_8100_median', 'tas_annual_0120_median')){
for (i in c('tas_annual_0120_median')){
  suitability_points <- calculate_distance(
    temp_bng, grid_size=1000, type='area', save_name=paste('./Data/Tif/', i, sep = ''),
    max_dist=5000, suitability_type='decrease', area_value = i)
}

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

tmap_mode("view")
tmap_mode("plot")
wind_map <- tm_shape(wind_bng) +
  tm_polygons(
    c("ws_spring_baseline_median", "ws_summer_baseline_median", "ws_autumn_baseline_median", "ws_winter_baseline_median"),
    border.alpha = 0, palette = "YlOrRd", style = "cont",
    breaks = c(0, 14), title = "Wind Speed (m/s)") +
  tm_facets(nrow = 2, ncol = 2, free.scales = FALSE) +
  tm_layout(
    panel.labels = c("Spring", "Summer", "Autumn", "Winter"),
    legend.outside = TRUE,
    legend.outside.position = "right",
    main.title = "Seasonal Wind Speed Distribution",
    main.title.position = "center") +
  tm_check_fix()

tmap_save(wind_map, filename = "Data/Layout/map wind speed.png",
          width = 10, height = 8, units = "in", dpi = 300,
          device = ragg::agg_png)


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

tmap_mode("plot")
drought_map <- tm_basemap("CartoDB.Positron") +
  tm_shape(drought_data) +
  tm_polygons(
    c('DSI12_baseline_81_00_median', 'DSI12_baseline_00_17_median'),
    title = 'DSI',
    palette = "YlOrRd",
    border.alpha = 0,
    style = "cont",
    breaks = c(0, 14)) +
  tm_facets(ncol = 2, free.scales = FALSE) +
  tm_layout(
    panel.labels = c("1981-2000", "2001-2017"),
    legend.outside = FALSE,
    legend.position = c("right", "top"),
    legend.bg.color = "white",
    legend.bg.alpha = 0.6,
    legend.frame = TRUE,
    main.title = "Drought Severity Index (12-Month Accumulations)")

tmap_save(drought_map, filename = "Data/Layout/map dsi.png",
          width = 10, height = 8, units = "in", dpi = 300,
          device = ragg::agg_png)

# for (i in c('DSI12_baseline_81_00_median', 'DSI12_baseline_00_17_median')){
for (i in c('DSI12_baseline_00_17_median')){
  choose <- i
  suitability_points <- calculate_distance(
    drought_data, grid_size=1000, type='area', save_name=paste('./Data/Tif/', choose, sep = ''),
    max_dist=5000, suitability_type='decrease', area_value = choose)
}

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

target_levels <- c("at least 95%", "at least 70%", "at least 50%", "at least 30%", "less than 30%", "update")
water_plot <- water_simple %>%
  mutate(resavail = factor(resavail, levels = target_levels))
colors <- c("#1a9641","#a6d96a", "#ffffbf", "#fdae61", "#d7191c", "#bababa")

tmap_mode("plot")
water_map <- tm_basemap("CartoDB.Positron") +
  tm_shape(water_plot) +
  tm_polygons(col = "resavail",
              style = "cat",
              palette = colors,
              title = "Resource Availability",
              alpha = 0.7,
              colorNA = "white",
              border.alpha = 0)+
  tm_layout(legend.position = c("right", "top"),
            legend.title.size = 0.7,
            legend.text.size = 0.55)

tmap_save(water_map, filename = "Data/Layout/map water resource.png",
            width = 10, height = 8, units = "in", dpi = 300,
            device = ragg::agg_png)

choose <- 'resavail'
calculate_distance(
  water_simple, grid_size=1000, type='area', save_name=paste('./Data/Tif/', choose, sep = ''),
  max_dist=5000, suitability_type='decrease', area_value = choose)

################################ EDA ################################

# temp_name
temp_bng%>%
  ggplot() +
  geom_histogram(aes(x = tas_annual_8100_median, fill = '1981-2000'), binwidth = 0.1, alpha = 0.5) +
  geom_histogram(aes(x = tas_annual_0120_median, fill = '2001-2020'), binwidth = 0.1, alpha = 0.5) +
  labs(title = "Annual Average Air Temperature (°C)",
       x = "Annual Average Air Temperature (°C)",
       y = "Count",
       fill = "Time Period")+
  scale_fill_manual(values = c("1981-2000" = "blue", "2001-2020" = "red"))+
  theme_minimal()

# e.g. when the global average temperature reaches 2.0 c, how much warmer the area is projected to b
plot_data <- temp_bng %>%
  st_drop_geometry() %>%
  dplyr::select(starts_with("tas_annual_") & -matches("8100|0120"))%>%
  pivot_longer(cols = everything(), names_to = "raw_name", values_to = "value")%>%
  mutate(
    gwl = str_extract(raw_name, "(?<=tas_annual_)[0-9]+"),
    stat = str_extract(raw_name, "[a-z]+$")) %>%
  mutate(
    gwl_num = as.numeric(gwl),
    gwl_label = case_when(
      gwl == "15" ~ "1.5°C", gwl == "2"  ~ "2.0°C", gwl == "25" ~ "2.5°C",
      gwl == "3"  ~ "3.0°C", gwl == "35" ~ "3.5°C", gwl == "4"  ~ "4.0°C"))%>%
  pivot_wider(id_cols = c(gwl_label, gwl_num), names_from = stat,
              values_from = value, values_fn = mean) %>%
  arrange(gwl_num)

correct_order <- c("1.5°C", "2.0°C", "2.5°C", "3.0°C", "3.5°C", "4.0°C")
plot_data$gwl_label <- factor(plot_data$gwl_label, levels = correct_order)

ggplot(plot_data, aes(x = gwl_label)) +
  geom_boxplot(
    aes(ymin = lower,lower = lower, middle = median, upper = upper, ymax = upper),
    stat = "identity", fill = "orange", alpha = 0.5, width = 0.5) +
  labs(
    title = "Annual Temperature Change by Global Warming Level",
    x = "Global Warming Level",
    y = "Temperature Change (°C)") +
  theme_minimal()


# wind_bng
wind_bng
wind_bng%>%
  ggplot() +
  geom_histogram(aes(x = ws_summer_baseline_median, fill = 'summer'), binwidth = 0.1, alpha = 0.5) +
  geom_histogram(aes(x = ws_winter_baseline_median, fill = 'winter'), binwidth = 0.1, alpha = 0.5) +
  geom_histogram(aes(x = ws_autumn_baseline_median, fill = 'autumn'), binwidth = 0.1, alpha = 0.5) +
  geom_histogram(aes(x = ws_spring_baseline_median, fill = 'spring'), binwidth = 0.1, alpha = 0.5) +
  labs(title = "Seasonal Average Wind Speed (m/s)",
       x = "Seasonal Average Wind Speed (m/s)",
       y = "Count",
       fill = "Season")+
  scale_fill_manual(values = c("summer" = "blue", "winter" = "red",
                               "autumn" = 'green', "spring" = 'yellow')) +
  theme_minimal()

# e.g. when the global average temperature reaches 2.0 c, how much windier the area is projected to be
wind_bng%>%
  ggplot() +
  geom_histogram(aes(x = ws_summer_20_median, fill = 'summer 2.0°C'), binwidth = 0.3, alpha = 0.5) +
  geom_histogram(aes(x = ws_winter_20_median, fill = 'winter 2.0°C'), binwidth = 0.3, alpha = 0.5) +
  geom_histogram(aes(x = ws_autumn_20_median, fill = 'autumn 2.0°C'), binwidth = 0.3, alpha = 0.5) +
  geom_histogram(aes(x = ws_spring_20_median, fill = 'spring 2.0°C'), binwidth = 0.3, alpha = 0.5) +
  labs(title = "Seasonal Average Wind Speed (m/s) at 2.0°C Global Warming Level",
       x = "Seasonal Average Wind Speed (m/s)",
       y = "Count",
       fill = "Season")+
  scale_fill_manual(values = c("summer 2.0°C" = "blue", "winter 2.0°C" = "red",
                               "autumn 2.0°C" = 'green', "spring 2.0°C" = 'yellow')) +
  theme_minimal()

# 4.0
wind_bng%>%
  ggplot() +
  geom_histogram(aes(x = ws_summer_40_median, fill = 'summer 4.0°C'), binwidth = 0.3, alpha = 0.5) +
  geom_histogram(aes(x = ws_winter_40_median, fill = 'winter 4.0°C'), binwidth = 0.3, alpha = 0.5) +
  geom_histogram(aes(x = ws_autumn_40_median, fill = 'autumn 4.0°C'), binwidth = 0.3, alpha = 0.5) +
  geom_histogram(aes(x = ws_spring_40_median, fill = 'spring 4.0°C'), binwidth = 0.3, alpha = 0.5) +
  labs(title = "Seasonal Average Wind Speed (m/s) at 4.0°C Global Warming Level",
       x = "Seasonal Average Wind Speed (m/s)",
       y = "Count",
       fill = "Season")+
  scale_fill_manual(values = c("summer 4.0°C" = "blue", "winter 4.0°C" = "red",
                               "autumn 4.0°C" = 'green', "spring 4.0°C" = 'yellow')) +
  theme_minimal()

# drought_data
# calculated with 12-month rainfall deficits provided as a percentage of the mean annual
# climatological total rainfall (1981–2000 and 2020-2017) for that location.
# It measures the severity of a drought, not the frequency.

# Projections is based on 2000-2020

drought_data%>%
  ggplot() +
  geom_histogram(aes(x = DSI12_baseline_81_00_median, fill = "1981-2000"),
                 binwidth = 0.1, alpha = 0.5) +
  geom_histogram(aes(x = DSI12_baseline_00_17_median, fill = "2000-2017"),
                 binwidth = 0.1, alpha = 0.5) +
  labs(title = "Drought Severity Index (12-Month Accumulations)",
       x = "Drought Severity Index",
       y = "Count",
       fill = "Time Period")+
  scale_fill_manual(values = c("1981-2000" = "blue", "2000-2017" = "red")) +
  theme_minimal()

# e.g. when the global average temperature reaches 1.5 c, how much drier the area is projected to be
drought_data%>%
  ggplot() +
  geom_histogram(aes(x = DSI12_15_median, fill = "1.5°C"),
                 binwidth = 0.3, alpha = 0.5) +
  geom_histogram(aes(x = DSI12_20_median, fill = "2.0°C"),
                 binwidth = 0.3, alpha = 0.5) +
  geom_histogram(aes(x = DSI12_25_median, fill = "2.5°C"),
                 binwidth = 0.3, alpha = 0.5) +
  geom_histogram(aes(x = DSI12_30_median, fill = "3.0°C"),
                 binwidth = 0.3, alpha = 0.5) +
  geom_histogram(aes(x = DSI12_40_median, fill = "4.0°C"),
                 binwidth = 0.3, alpha = 0.5) +
  labs(title = "Drought Severity Index (12-Month Accumulations)",
       x = "Drought Severity Index",
       y = "Count",
       fill = "Global Warming Level")+
  scale_fill_manual(values = c("1.5°C" = "blue", "2.0°C" = "red",
                               "2.5°C" = 'green', "3.0°C" = 'yellow',
                               "4.0°C" = 'purple')) +
  theme_minimal()


plot_data <- drought_data %>%
  st_drop_geometry() %>%
  dplyr::select(matches("DSI12_[0-9]+_")) %>%
  pivot_longer(cols = everything(), names_to = "raw_name", values_to = "value") %>%
  mutate(
    gwl = str_extract(raw_name, "(?<=DSI12_)[0-9]+"), stat = str_extract(raw_name, "[a-z]+$")) %>%
  mutate(
    stat = ifelse(stat == "low", "lower", stat),
    gwl_num = as.numeric(gwl),
    gwl_label = case_when(
      gwl == "15" ~ "1.5°C", gwl == "20"  ~ "2.0°C", gwl == "25" ~ "2.5°C",
      gwl == "30"  ~ "3.0°C", gwl == "40"  ~ "4.0°C"))%>%
  pivot_wider(id_cols = c(gwl_label, gwl_num), names_from = stat,
              values_from = value, values_fn = mean) %>%
  arrange(gwl_num)

correct_order <- c("1.5°C", "2.0°C", "2.5°C", "3.0°C", "4.0°C")
plot_data$gwl_label <- factor(plot_data$gwl_label, levels = correct_order)

ggplot(plot_data, aes(x = gwl_label)) +
  geom_boxplot(
    aes(ymin = lower,lower = lower, middle = median, upper = upper, ymax = upper),
    stat = "identity", fill = "orange", alpha = 0.5, width = 0.5) +
  labs(
    title = "Drought Severity Index (12-Month Accumulations) by Global Warming Level",
    x = "Global Warming Level",
    y = "Drought Severity Index") +
  theme_minimal()
