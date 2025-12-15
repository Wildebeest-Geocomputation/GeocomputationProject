library(sf)
library(dplyr)
library(terra)
library(geodata)
library(stringr)

# Load and preprocess data
setwd("C:\\Users\\alfat\\Documents\\STA_MSc\\R\\Group Assignment\\Data\\road_networks\\Download_2869127\\open-roads_6194291")

road_files <- list.files(pattern = "*_RoadLink\\.shp$",
                         full.names = TRUE,
                         recursive = FALSE) # Read all RoadLink shapefiles

all_roads <- do.call(rbind, lapply(road_files, st_read))

strategic_roads <- all_roads %>%
  filter(class == "Motorway" |
           (class == "A Road" & primary == "true")) # Filter for motorways and primary A-roads


# Read UK boundary
uk_boundary <- st_read("C:\\Users\\alfat\\Documents\\STA_MSc\\R\\Group Assignment\\Data\\road_networks\\Local_Authority_Districts_May_2024_Boundaries_UK_BGC_-5850961694214429102\\LAD_MAY_2024_UK_BGC.shp")

england_boundary <- uk_boundary %>%
  filter(str_detect(LAD24CD, "^E")) # Filter for England only
# Clip to England only
england_strategic <- st_intersection(strategic_roads, england_boundary)

# Remove any NA values in class column
england_strategic <- england_strategic %>%
  filter(!is.na(class))

grid_res <- 1000 # 1km
r_grid <- rast(ext(england_boundary), resolution = grid_res, crs = crs(england_boundary))
values(r_grid) <- 1:ncell(r_grid)
r_grid_masked <- mask(r_grid, england_boundary)

# Calculate Euclidean distance to roads
r_lines <- rasterize(england_strategic, r_grid_masked, field=1)
dist_grid <- distance(r_lines)

# Apply fuzzy membership function
# Define fuzzy function (closer is better - decreasing)
fuzzy_decrease <- function(x, max_dist) {
  val <- 1 - (x / max_dist)
  val[val < 0] <- 0
  return(val)
}

# Apply function (max distance 10km)
suitability_roads <- app(dist_grid, fun = function(x) fuzzy_decrease(x, max_dist = 10000))

# Visualise
plot(suitability_roads, main = "Major Road Network Proximity (England)")
lines(england_strategic, col="blue", lwd=0.5)

# Save the raster for MaxEnt
writeRaster(suitability_roads,
            "C:\\Users\\alfat\\Documents\\STA_MSc\\R\\Group Assignment\\Data\\road_network_proximity.tif",
            overwrite=TRUE)

