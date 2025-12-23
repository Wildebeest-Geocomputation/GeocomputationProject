library(sf)
library(terra)
source('~/GeocomputationProject/utils/boundaries.R')
source('~/GeocomputationProject/utils/fuzzy.R')

# Load road data
eng_roads=st_read("~/STA_MSc/Term_1/SAGc/Group assignment/Data/strategic_rds.shp")

# Make the empty raster
grid_res <- 1000 # 1km
r_grid <- rast(ext(england_bng), resolution = grid_res, crs = crs(england_bng))
values(r_grid) <- 1:ncell(r_grid)
r_grid_masked <- mask(r_grid, england_bng)

# Calculate Euclidean distance to roads
r_lines <- rasterize(eng_roads, r_grid_masked, field=1)
dist_grid <- distance(r_lines)

# Apply decreasing function (max distance 10km)
suitability_roads <- app(dist_grid, fun = function(x) fuzzy_decrease(x, max_dist = 10000))
suitability_roads <- mask(suitability_roads, england_bng)

# Visualise
plot(suitability_roads, main = "Major Road Network Proximity (England)")
#lines(eng_roads, col="blue", lwd=0.5) # to show the roads

# Save the raster for MaxEnt
writeRaster(suitability_roads,
            "~/GeocomputationProject/Data/Tif/road_network.tif",
            overwrite=TRUE)

