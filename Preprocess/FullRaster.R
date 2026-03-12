source('utils/fullpreprocess.R')
source('utils/boundaries.R')


for (i in c('TAS Annual 2001-2020 median')){
  suitability_points <- calculate_distance(
    temp_bng, grid_size=1000, type='area', #save_name=paste('./Data/Tif/', i, sep = ''),
    # save_name='./Data/Tif/Annual_Median_Temperature_2001_2020',
    max_dist=10000, suitability_type='decrease', area_value = i)
}


eng_geology <- st_read("./MaxEnt_data/geology_best.shp")
eng_flood_risk <- st_read("./MaxEnt_data/Flood_Risk_Areas.shp")
eng_solar <- rast("./MaxEnt_data/solar_irradiation_england.tif") # This need to be add

temp <- st_read("./PData/Individual/TempWater/annual_average_temperature_change_projections_local_authority_v1_-8116657329075127237/annual_average_temperature_change_projections_local_authority_v1.shp")
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
temp_bng <- temp %>% filter(str_starts(CODE, "E"))%>%st_transform(27700)

file_path <- './PData/Individual/TempWater/Drought_Severity_Index_12_Month_Accumulations_1364492214063738842.gpkg'
st_layers(file_path)
drought_data <- st_read(file_path)

brownfield <- read_csv('./PData/Individual/brownfield-land.csv')
brownfield_england <- brownfield%>%
  filter(!is.na(point)) %>%
  st_as_sf(wkt = 'point', crs = 4326)%>%
  st_transform(crs = 27700)%>%
  st_filter(st_as_sf(england_bng))

eng_roads <- st_read("./MaxEnt_data/strategic_rds.shp")
emp_5000 = st_read("./MaxEnt_data/emp_5000.gpkg") # Employment need to be add

grid_size <- 1000
# Geology
suitability_points <- calculate_distance(
  eng_geology, grid_size=grid_size, type='point', #save_name=paste('./Data/Tif/', i, sep = ''),
  # save_name='./Data/Tif/Annual_Median_Temperature_2001_2020',
  max_dist=10000, suitability_type='decrease')
# Flood Risk
suitability_points <- calculate_distance(
  eng_flood_risk, grid_size=grid_size, type='point', #save_name=paste('./Data/Tif/', i, sep = ''),
  # save_name='./Data/Tif/Annual_Median_Temperature_2001_2020',
  max_dist=10000, suitability_type='increase')
# Temperature
suitability_points <- calculate_distance(
  temp_bng, grid_size=grid_size, type='area', #save_name=paste('./Data/Tif/', i, sep = ''),
  # save_name='./Data/Tif/Annual_Median_Temperature_2001_2020',
  max_dist=10000, suitability_type='decrease', area_value = 'TAS Annual 2001-2020 median')
# Drought
suitability_points <- calculate_distance(
  drought_data, grid_size=grid_size, type='area', #save_name=paste('./Data/Tif/', choose, sep = ''),
  # save_name = './Data/Tif/Drought_Median_2000_2017',
  max_dist=10000, suitability_type='decrease', area_value = 'DSI12_baseline_00_17_median')
# Brownfield
suitability_points <- calculate_distance(
  brownfield_england, grid_size=grid_size, type='point', #save_name='./Data/Tif/brownfield',
  max_dist=5000, suitability_type='decrease')
# Roads
suitability_points <- calculate_distance(
  eng_roads, grid_size=grid_size, type='point', #save_name='./Data/Tif/brownfield',
  max_dist=5000, suitability_type='decrease')





