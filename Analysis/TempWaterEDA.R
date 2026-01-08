source('utils/boundaries.R')
england_bng
data_centers_sf <- read_csv("Data/Example/UK_Data_Centers.csv") %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326) %>%
  st_transform(27700)

temp <- st_read("./PData/Individual/TempWater/annual_average_temperature_change_projections_local_authority_v1_-8116657329075127237/annual_average_temperature_change_projections_local_authority_v1.shp")
temp_name <- read_csv("./PData/Individual/TempWater/annual_average_temperature_change_projections_local_authority_v1_-7277948155124131527.csv")
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
boundary_temp <- temp_bng %>%
  left_join(temp_name, by = "CODE")
final_table <- boundary_temp %>%
  mutate(dc_count = lengths(st_intersects(., data_centers_sf)))

brownfield <- read_csv('./PData/Individual/brownfield-land.csv')%>%
  filter(!is.na(point)) %>%
  st_as_sf(wkt = 'point', crs = 4326)%>%
  st_transform(crs = 27700)

final_table <- final_table %>%
  mutate(brownfield_count = lengths(st_intersects(., brownfield)))


file_path <- './PData/Individual/TempWater/Drought_Severity_Index_12_Month_Accumulations_1364492214063738842.gpkg'
st_layers(file_path)
drought_data <- st_read(file_path)%>%
  st_transform(crs = 27700)

final_table <- final_table %>%
  st_join(drought_data) %>%
  group_by(CODE) %>%
  mutate(
    avg_DSI12_baseline = mean(DSI12_baseline_00_17_median, na.rm = TRUE)
  ) %>%
  slice(1) %>%
  ungroup()

final_df <- final_table %>%
  st_drop_geometry() %>%
  dplyr::select(
    CODE,
    dc_count,
    brownfield_count,
    avg_DSI12_baseline,
    `TAS Annual 2001-2020 median.x`
  )
final_df[is.na(final_df)] <- 0


library(corrplot)

cor_data <- final_table %>%
  st_drop_geometry() %>%
  dplyr::select(
    `Data Centers` = dc_count,
    `Brownfield Sites` = brownfield_count,
    `Drought Index (DSI12)` = avg_DSI12_baseline
  ) %>%
  mutate(across(everything(), as.numeric)) %>%
  na.omit()

library(GGally)
cor_data_ext <- final_table %>%
  st_drop_geometry() %>%
  dplyr::select(
    `Data Centers` = dc_count,
    `Brownfield` = brownfield_count,
    `Drought` = avg_DSI12_baseline,
    `Temp` = `TAS Annual 2001-2020 median.x`
  ) %>%
  mutate(across(everything(), as.numeric)) %>%
  na.omit()

ggpairs(cor_data_ext,
        title = "Scatter Plot Matrix with Correlation Coefficients",
        upper = list(continuous = wrap("cor", method = "spearman", size = 4, color = "black")),
        lower = list(continuous = wrap("smooth", alpha = 0.3, size = 0.1)))


cor_data_ext%>%
  ggplot() +
  geom_point(aes(x = `Brownfield`, y = `Temp`, colour = `Data Centers`)) +
  scale_color_viridis_c() +
  theme_minimal()
