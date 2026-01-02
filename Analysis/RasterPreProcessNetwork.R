# This is only an example code to preprocess raster data based on network, since it's using the boundaries of England level 2
# it doesn't represent anything important to the data centre example.

# if (!require("geodata")) install.packages("geodata")
# if (!require("terra")) install.packages("terra")

library(terra)
source('utils/fuzzy.R')
source('utils/boundaries.R')

plot(england_bng, main="England Boundary")
# plot(england_l2_bng, main="England Level 2 Boundary")

r_grid <- rast(ext(england_bng), resolution = grid_res, crs = crs(england_bng))
# This is to get the actual grid cells only within England
# r_grid_masked <- mask(r_grid, england_bng)

lines_latlon <- as.lines(england_bng)
power_lines_bng <- project(lines_latlon, crs(r_grid))

r_points <- rasterize(power_lines_bng, r_grid, field = 1, touches = TRUE)
dist_grid <- distance(r_points)
plot(dist_grid)
lines(power_lines_bng, col="red", lwd=2)

suitability_networks <- app(dist_grid, fun = function(x) fuzzy_decrease(x, max_dist = 10000))
suitability_networks <- mask(suitability_networks, england_bng)
plot(suitability_networks)

# testing function from FullProcess
source('Analysis/FullPreprocess.R')
suitability_networks <- calculate_distance(england_bng, grid_size=1000, type='network', save_name='./Data/Tif/network', max_dist=10000)
