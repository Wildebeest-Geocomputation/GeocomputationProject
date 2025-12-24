library(sf)
library(terra)
library(tidyverse)
source('utils/fuzzy.R')
source('utils/boundaries.R')

r_grid <- rast(ext(england_bng), resolution = 1000, crs = crs(england_bng))

brownfield <- read_csv("./PData/brownfield-site.csv")
brownfield_england <- brownfield%>%
  st_as_sf(wkt = 'geometry', crs = 4326)%>%
  st_transform(crs = 27700)%>%
  st_filter(st_as_sf(england_bng))

plot(brownfield_england)

# raster ize points to raster grid, it will assign value 1 to the cells with points
r_points <- rasterize(vect(brownfield_england), r_grid, field = 1)
dist_grid <- distance(r_points)
plot(dist_grid)
suitability_points <- app(dist_grid, fun = function(x) fuzzy_decrease(x, max_dist = 5000))
suitability_points <- mask(suitability_points, england_bng)

plot(suitability_points)

# testing function from FullProcess
source('Analysis/FullPreprocess.R')
suitability_points <- calculate_distance(brownfield_england, grid_size=1000, type='point', save_name='./Data/Tif/brownfield', max_dist=5000, suitability_type='decrease')
