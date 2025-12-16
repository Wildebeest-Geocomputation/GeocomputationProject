<<<<<<< HEAD
library(sf)
library(maxnet)

presence <- c(suitability_points, suitability_networks)
names(presence) <- c("points", "networks")

data_centers_sf <- read_csv("Data/Example/UK_Data_Centers.csv") %>%
=======
# 1. suitability_power
# 2. suitability_roads
presence <- c(suitability_power, suitability_roads)
names(presence) <- c("power", "roads")

library(sf)
library(maxnet)
library(tidyverse)

data_centers_sf <- read_csv("data/UK_Data_Centers.csv") %>%
>>>>>>> 2ddacb53eaed5f44aa7c199db6eaebe772c49923
  st_as_sf(coords = c("lon", "lat"), crs = 4326) %>%
  st_transform(27700)

# This is to get the actual raster values for each data center location
<<<<<<< HEAD
# tidyr has a conflict with terra, so use :: to specify
presence_vals <- terra::extract(presence, data_centers_sf, ID = FALSE)
=======
presence_vals <- extract(presence, data_centers_sf, ID = FALSE)
>>>>>>> 2ddacb53eaed5f44aa7c199db6eaebe772c49923

# This is to not getting error for maxent, there ar na now is because I crop the raster, so some data center doesn't have value
valid_rows <- complete.cases(presence_vals)
presence_clean <- presence_vals[valid_rows, ]

bg_data <- spatSample(presence, size = 1000, method = "random", na.rm = TRUE, values = TRUE)
# bg_data <- as.data.frame(bg_data)
model_data <- rbind(presence_clean, bg_data)

# The background point set to 0 because in entropy, presence points are 1, 0 represent random distributed points
pa <- c(rep(1, nrow(presence_clean)), rep(0, nrow(bg_data)))
<<<<<<< HEAD
me_model <- maxnet(p = pa, data = model_data)
suitability_map <- predict(presence, me_model, type = "logistic", na.rm = TRUE)

plot(me_model, type = "logistic")

png("Data/SuitibilityMap/data_center_suitability.png", width = 2000, height = 2000, res = 300)
plot(suitability_map, main = "Data Center Suitability")
dev.off()
=======

me_model <- maxnet(p = pa, data = model_data)
plot(me_model, type = "logistic")

suitability_map <- predict(presence, me_model, type = "logistic")
plot(suitability_map, main = "Data Center Suitability")
>>>>>>> 2ddacb53eaed5f44aa7c199db6eaebe772c49923
