# This example use crime data in england to process raster data.

library(sf)
library(terra)
library(tidyverse)
source('utils/fuzzy.R')
source('utils/boundaries.R')

r_grid <- rast(ext(england_bng), resolution = 1000, crs = crs(england_bng))

all_data <- read_csv("./Data/Example/crime_england_robbery_2025_09.csv")
crime_england <- all_data%>%
  # read the original data with it's crs, and transform to 27700 to fit boundary
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326)%>%
  st_transform(crs = 27700)%>%
  # find the points within England boundary, it's crs is already 27700
  st_intersection(st_as_sf(england_bng))

plot(crime_england)

# raster ize points to raster grid, it will assign value 1 to the cells with points
r_points <- rasterize(vect(crime_england), r_grid, field = 1)
dist_grid <- distance(r_points)
plot(dist_grid)
suitability_points <- app(dist_grid, fun = function(x) fuzzy_decrease(x, max_dist = 5000))
suitability_points <- mask(suitability_points, england_bng)

plot(suitability_points)

# testing function from FullProcess
source('Analysis/FullPreprocess.R')
suitability_points <- calculate_distance(crime_england, grid_size=1000, type='point', save_name='./Data/Tif/point', max_dist=5000)
