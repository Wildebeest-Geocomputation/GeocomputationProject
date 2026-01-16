# As of last run:
# AUC: 0.92295333333333
# Best AIC: 5264.12744728522
# Best model param RegMult: 0.1 Features: lq

library(sf)
library(tmap)
library(tools)
library(terra)
library(maxnet)
library(tidyverse)
source('utils/boundaries.R')
source('utils/model_performance.R')

# Define the paths for the two specific files
selected_tifs <- c("./Data/Tif/employment_accessibility_england.tif",
                   "./Data/Tif/road_network.tif")

presence <- rast(selected_tifs)

names(presence) <- file_path_sans_ext(basename(selected_tifs))

data_centers_sf <- read_csv("Data/Example/UK_Data_Centers.csv") %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326) %>%
  st_transform(27700)

# This is to get the actual raster values for each data center location
# tidyr has a conflict with terra, so use :: to specify
presence_vals <- terra::extract(presence, data_centers_sf, ID = FALSE)

# This is to not getting error for maxent, there are na now is because I crop the raster, so some data center doesn't have value
valid_rows <- complete.cases(presence_vals)
presence_clean <- presence_vals[valid_rows, , drop = FALSE]

set.seed(123)
bg_data <- spatSample(presence, size = 1000, method = "random", na.rm = TRUE, values = TRUE)
model_data <- as.data.frame(rbind(presence_clean, bg_data))

# The background point set to 0 because in entropy, presence points are 1, 0 represent random distributed points
pa <- c(rep(1, nrow(presence_clean)), rep(0, nrow(bg_data)))
# There is a bug in model, if the input only contain 1 variable, it will cause error bacause  [drop = FALSE]
me_model <- maxnet(p = pa, data = model_data)
suitability_map <- predict(presence, me_model, type = "logistic", na.rm = TRUE)
plot(suitability_map)

# updated maxent plot
final_map <- tm_shape(suitability_map) +
  tm_raster(style = "cont", palette = "viridis", alpha = 0.9, title = "Suitability") +
  tm_compass(position = c("right", "top")) +
  tm_scale_bar(position = c("right", "bottom")) +
  tm_grid(labels.size = 0.7, n.x = 5, n.y = 5,
          lwd = 0.1,
          alpha = 0.1,
          labels.inside.frame = FALSE)+
  tm_layout(
    main.title.size = 1,
    legend.outside = FALSE,
    legend.position = c("left", "top"),
    legend.bg.color = "white",
    legend.bg.alpha = 0.5,
    legend.frame = TRUE
  )
tmap_save(final_map, filename = "~/GeocomputationProject/Data/SuitibilityMap/road_data_center_suitability.png", width = 10, height = 8)

# This is to find the best model params using grid search,
regmult_vals <- c(0.1, 0.5, 1)
feature_classes <- c("l", "q", "h", "t", "lq")
grid_search_result <- grid_search(
  data = model_data,
  pa = pa,
  regmult_vals = regmult_vals,
  feature_classes = feature_classes
)

me_model <- grid_search_result$best_model

message(paste("Best AIC:", grid_search_result$best_score))
message(paste("Best model param RegMult:", grid_search_result$best_params[1],
              "Features:", grid_search_result$best_params[2]))

# This is suitability map
png("Data/SuitibilityMap/road_data_center_suitability.png", width = 2000, height = 2000, res = 300)
plot(suitability_map, main = "Data Center Suitability by Connectivity Criteria")
dev.off()

# This is response curve
png("Data/SuitibilityMap/road_model_response.png", width = 3500, height = 2000, res = 300)
plot(me_model, type = "logistic")
dev.off()

response.plot(me_model, names(model_data)[1], type = "logistic")

report <- maxent_model_report(me_model, model_data, pa_vector = pa)

tmap_mode("plot")
suitability_map <- predict(presence, me_model, type = "logistic", na.rm = TRUE)
threshold <- report$youden$threshold
suitability_map[suitability_map <= threshold] <- NA
smap <- tm_basemap("CartoDB.Positron") +
  tm_shape(suitability_map) +
  tm_raster(title = "Suitability", palette = "viridis", alpha = 0.7)+
  tm_shape(england_bng) +
  tm_borders(col = "black", alpha = 0.3, lwd = 0.2)

tmap_save(smap,
          filename = paste0("Data/SuitibilityMap/road_data_center_suitability_", threshold, ".png"),
          width = 10, height = 8, units = "in", dpi = 300,
          device = ragg::agg_png)
