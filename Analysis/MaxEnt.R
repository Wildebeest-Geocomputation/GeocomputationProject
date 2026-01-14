library(sf)
library(tmap)
library(tools)
library(terra)
library(maxnet)
library(tidyverse)
source('utils/boundaries.R')
source('utils/model_performance.R')

tif_files <- list.files(path = "./Data/Tif",
                        pattern = "\\.tif$",
                        full.names = TRUE,
                        ignore.case = TRUE)

presence <- rast(tif_files)%>%
  terra::classify(cbind(NA, 0))%>%
  terra::mask(england_bng)
plot(presence)

names(presence) <- file_path_sans_ext(basename(tif_files))

data_centers_sf <- read_csv("Data/Example/UK_Data_Centers.csv") %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326) %>%
  st_transform(27700)

# This is to get the actual raster values for each data center location
# tidyr has a conflict with terra, so use :: to specify
presence_vals <- terra::extract(presence, data_centers_sf, ID = FALSE)

# This is to not getting error for maxent, there ar na now is because I crop the raster, so some data center doesn't have value
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


crs(suitability_map) <- "EPSG:27700"
# suitability_longlat <- terra::project(suitability_map, "EPSG:4326")
# suitability_map[suitability_map == 0] <- NA
tmap_mode("plot")
tm_shape(suitability_map) +
  tm_raster(style = "cont", palette = "viridis", alpha = 0.9, title = "Suitability") +
  tm_compass(position = c("right", "top")) +
  tm_scale_bar(position = c("right", "top")) +
  tm_grid(labels.size = 0.7, n.x = 5, n.y = 5,
          lwd = 0.1,
          alpha = 0.5,
          labels.inside.frame = FALSE)+
  tm_layout(
    main.title.size = 1,
    legend.outside = FALSE,
    legend.position = c("left", "top"),
    legend.bg.color = "white",
    legend.bg.alpha = 0.5,
    legend.frame = TRUE
  )

# This is to find the best model params using grid search,
# this is based on AIC score to find the best model,
# you can find formula in utils
regmult_vals <- c(0.05, 0.1, 0.5, 1)
feature_classes <- c("l", "q", "h", "p", "t", "lq", "lh", "lp", "lt")
grid_search_result <- grid_search(
  data = model_data,
  pa = pa,
  regmult_vals = regmult_vals,
  feature_classes = feature_classes
)
me_model <- grid_search_result$best_model
grid_search_result$best_params
# Look at the model performance to see the importance of each variable
print(me_model$betas)

message(paste("best AIC:", grid_search_result$best_score))
message(paste("Best model param RegMult:", grid_search_result$best_params[1],
              "Features:", grid_search_result$best_params[2]))

# This is response curve
png("Data/SuitibilityMap/model_importance.png", width = 2000, height = 2000, res = 300)
plot(me_model, type = "logistic")
dev.off()

png("Data/SuitibilityMap/data_center_suitability.png", width = 2000, height = 2000, res = 300)
plot(suitability_map, main = "Data Center Suitability")
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
          filename = paste0("Data/SuitibilityMap/suitability_map_", threshold, ".png"),
          width = 10, height = 8, units = "in", dpi = 300,
          device = ragg::agg_png)





