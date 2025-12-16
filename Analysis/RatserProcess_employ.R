library(sf)
library(terra)
source('~/GeocomputationProject/utils/boundaries.R')

#read data
emp_5000 = st_read("~/STA_MSc/Term_1/SAGc/Group assignment/Data/emp_5000.gpkg")

# Create raster grid (1km resolution to match other maps)
grid_res <- 1000  # 1km
r_grid <- rast(ext(england_bng), resolution = grid_res, crs = crs(england_bng))

# Rasterize the data
employment_raster <- rasterize(
  vect(emp_5000),
  r_grid,
  field = "X5000EmpAvgt",
  fun = "mean"
)

# Mask to England boundary
employment_raster <- mask(employment_raster, england_bng)

employment_raster_filled <- focal(
  employment_raster,
  w = 5,
  fun = "mean",
  na.policy = "only",  # Only fill NA cells
  na.rm = TRUE
)

# normalize
min_time <- minmax(employment_raster_filled)[1]
max_time <- minmax(employment_raster_filled)[2]
suitability_employment <- 1 - ((employment_raster_filled - min_time) / (max_time - min_time))

# Plot
plot(suitability_employment,
     main = "Average Time to Large Employment Centres",
     col = hcl.colors(100, "viridis", rev = FALSE))

# Save the raster
writeRaster(suitability_employment,
            "~/GeocomputationProject/Data/Tif/employment_accessibility_england.tif",
            overwrite = TRUE)
