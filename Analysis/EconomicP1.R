library(sf)
library(terra)
source('C:/Temp_R/fuzzy.R')
source('C:/Temp_R/boundaries.R')

# Load geology data
# SAND AND GRAVEL and SAND provide good drainage and stable foundations
eng_geology <- st_read("C:/Temp_R/geology_best.shp")

# Make the empty raster
grid_res <- 1000 # 1km
r_grid <- rast(ext(england_bng), resolution = grid_res, crs = crs(england_bng))
values(r_grid) <- 1:ncell(r_grid)
r_grid_masked <- mask(r_grid, england_bng)

# Calculate Euclidean distance to most suitable soil types for data center foundations
r_lines <- rasterize(eng_geology, r_grid_masked, field=1)
dist_grid <- distance(r_lines)

# Apply decreasing function (max distance 5 km)
suitability_geology <- app(dist_grid, fun = function(x) fuzzy_decrease(x, max_dist = 5000))
suitability_geology <- mask(suitability_geology, england_bng)

# Visualise
plot(suitability_geology, main = "Soil suitability proximity (England)")
#lines(eng_roads, col="blue", lwd=0.5) # to show the roads

# Save the raster for MaxEnt
writeRaster(suitability_geology,
            "C:/Temp_R/geology_suitability(1).tif",
            overwrite=TRUE)
#_____________________________________________________________________________#

# Load flood risk data
eng_flood_risk <- st_read("C:/Temp_R/Flood_Risk_Areas.shp")

# Make the empty raster
grid_res <- 1000 # 1km
r_grid <- rast(ext(england_bng), resolution = grid_res, crs = crs(england_bng))
values(r_grid) <- 1:ncell(r_grid)
r_grid_masked <- mask(r_grid, england_bng)

# Calculate Euclidean distance to areas farther from flood risk zones
r_lines <- rasterize(eng_flood_risk, r_grid_masked, field=1)
dist_grid <- distance(r_lines)

# Apply increasing function (min distance away from flood zones = 4km)
suitability_flood <- app(dist_grid, fun = function(x) fuzzy_increase(x, max_dist = 4000))
suitability_flood <- mask(suitability_flood, england_bng)

# Visualise
plot(suitability_flood, main = "Areas outside flood‑risk zones (England)")
#lines(eng_roads, col="blue", lwd=0.5) # to show the roads

# Save the raster for MaxEnt
writeRaster(suitability_flood,
            "C:/Temp_R/non_floodrisk_suitability(1).tif",
            overwrite=TRUE)

#____________________________________________________________________________#

# Load solar irradiation data (already a raster)
eng_solar <- rast("C:/Temp_R/solar_irradiation_england.tif")

# Reproject solar data to match england_bng CRS
eng_solar_projected <- project(eng_solar, crs(england_bng), method = "bilinear")

# Resample to match analysis grid
eng_solar_resampled <- resample(eng_solar_projected, r_grid_masked, method = "bilinear")

# Crop AND mask to England boundaries
eng_solar_cropped <- crop(eng_solar_resampled, england_bng)
eng_solar_masked <- mask(eng_solar_cropped, england_bng)

# Define threshold for high solar irradiation zones
threshold <- 977  # kWh/m²/year

# Create binary raster: 1 = high irradiation zones, NA = rest
high_solar_zones <- ifel(eng_solar_masked >= threshold, 1, NA)

# Calculate Euclidean distance to high solar irradiation zones
dist_solar <- distance(high_solar_zones)

# Apply decreasing function (max distance 15 km)
suitability_solar <- app(dist_solar, fun = function(x) fuzzy_decrease(x, max_dist = 15000))

# Crop AND mask final result
suitability_solar_cropped <- crop(suitability_solar, england_bng)
suitability_solar <- mask(suitability_solar_cropped, england_bng)

# Visualise
plot(suitability_solar, main = "Solar Irradiation Proximity Suitability (England)")
#plot(england_bng, add = TRUE, border = "black", lwd = 1)

# Save the raster for MaxEnt
writeRaster(suitability_solar,
            "C:/Temp_R/solar_suitability(1).tif",
            overwrite=TRUE)
