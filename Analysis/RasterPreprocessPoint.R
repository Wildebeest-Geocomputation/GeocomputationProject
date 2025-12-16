<<<<<<< HEAD
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
=======
library(terra)
library(geodata)

uk_level1 <- gadm(country="GBR", level=1, path=tempdir())
uk_level1

uk_level2 <- gadm(country="GBR", level=2, path=tempdir())
others <- c("Scotland", "Wales", "Northern Ireland")
# some england data are na value
england_counties <- uk_level2[!uk_level2$NAME_1 %in% others, ]

england <- aggregate(england_counties)
england_bng <- project(england, "EPSG:27700")
plot(england_bng, main="England Boundary")

grid_res <- 20000 # 20km

r_grid <- rast(ext(england_bng), resolution = grid_res, crs = crs(england_bng))
values(r_grid) <- 1:ncell(r_grid)
r_grid_masked <- mask(r_grid, england_bng)
v_grid <- as.polygons(r_grid, extent = FALSE)
v_grid_clipped <- crop(v_grid, england_bng)


plot(r_grid_masked, main = paste("Raster Grid:", grid_res/1000, "km"), axes=FALSE)
plot(r_grid_masked,
     main = paste("Raster Grid:", grid_res/1000, "km"),
     axes = FALSE,
     col = "grey90",
     legend = FALSE)
lines(england_bng, lwd=1)

plot(england_bng, main = "Vector Mesh (Polygons)", col="grey90", border="grey")
lines(v_grid, col="red", lwd=1)

# find the nearest crime points to each grid cell center
all_data <- read_csv("./PData/Individual/all_crime_data_2025_09.csv")
crime_england <- all_data %>%
  filter(!is.na(Longitude) & Crime.type=='Robbery')%>%
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326)%>%
  st_transform(crs = 27700)%>%
>>>>>>> 2ddacb53eaed5f44aa7c199db6eaebe772c49923
  st_intersection(st_as_sf(england_bng))

plot(crime_england)

<<<<<<< HEAD
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
=======
crime_england_vect <- vect(crime_england)

dist_grid <- distance(r_grid_masked, crime_england_vect)
suitability_points <- app(dist_grid, fun = function(x) fuzzy_decrease(x, max_dist = 5000))
plot(suitability_points)

>>>>>>> 2ddacb53eaed5f44aa7c199db6eaebe772c49923
