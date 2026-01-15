library(sf)
library(terra)
library(tmap)
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
suitability_roads <- app(dist_grid, fun = function(x) fuzzy_decrease(x, max_dist = 5000))
suitability_roads <- mask(suitability_roads, england_bng)

#suitability_roads[!is.na(r_lines)] <- NA # because the roads themselves can't contain data centres

#============================================================================
# Employ raster process
#============================================================================

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

#=======================================================================
# Plot together
# 1. Define your shared layout style
# (This keeps the code DRY - Don't Repeat Yourself)
my_layout <- tm_layout(
  main.title.size = 1.2,
  main.title.position = "center",
  legend.position = c("left", "top"), # Inner legend
  legend.bg.color = "white",
  legend.bg.alpha = 0.6,
  legend.frame = TRUE,
  frame = TRUE
)

# 2. Create Map Object 1: Roads
map_roads <- tm_shape(suitability_roads) +
  tm_raster(style = "cont",
            palette = "viridis",
            alpha = 0.9,
            title = "Suitability Score",
            labels = c("Low", "", "", "", "", "High")) + # Change title to fit your metric
  tm_layout(main.title = "Proximity to Major Roads") +
  my_layout + # Apply the shared style defined above
  tm_compass(position = c("right", "top"), size = 2) +
  tm_scale_bar(position = c("right", "bottom"))

# 3. Create Map Object 2: Employment
map_emp <- tm_shape(suitability_employment) +
  tm_raster(style = "cont",
            palette = "viridis",
            alpha = 0.9,
            title = "Suitability Score",
            labels = c("Low", "", "", "", "", "High")) +
  tm_layout(main.title = "Time to Large Employers") +
  my_layout + # Apply the shared style again
  tm_compass(position = c("right", "top"), size = 2) +
  tm_scale_bar(position = c("right", "bottom"))

# ------------------------------------------------------------------------------
# TEAMMATE NOTE: TO ADD A 3RD MAP, UNCOMMENT AND EDIT THE BLOCK BELOW
# ------------------------------------------------------------------------------
# map_3 <- tm_shape(YOUR_NEW_RASTER_HERE) +           # <--- Insert 3rd raster name
#   tm_raster(style = "cont", palette = "viridis", alpha = 0.9,
#             title = "Suitability Score",
#             labels = c("Low", "", "", "", "", "High")) +
#   tm_layout(main.title = "Your Third Title Here") + # <--- Insert 3rd title
#   my_layout +                                       # <--- Keeps style consistent
#   tm_compass(position = c("right", "top"), size = 2) +
#   tm_scale_bar(position = c("right", "bottom"))
# ==============================================================================

# 4. Plot them together
# This creates the side-by-side view
# TEAMMATE NOTE: Add "map_3" to the list below.
# Change ncol to 3 if you want them all in one row.
final_map <- tmap_arrange(map_roads, map_emp, ncol = 2)
# final_map <- tmap_arrange(map_roads, map_emp, map_3, ncol = 3) # replace map_3 with your third map

# Display the map
final_map

# Save
tmap_save(final_map, filename = "~/GeocomputationProject/Data/SuitibilityMap/road_variable_suitability.png", width = 12, height = 8)
#=======================================================================

# Plot seperately
plot(suitability_employment,
     main = "Average Time to Large Employment Centres",
     col = hcl.colors(100, "viridis", rev = FALSE))

plot(suitability_roads, main = "Proximity to Major Roads")
#lines(eng_roads, col="blue", lwd=0.5) # to show the roads

# Save the rasters separately for MaxEnt
writeRaster(suitability_roads,
            "~/GeocomputationProject/Data/Tif/road_network.tif",
            overwrite=TRUE)
writeRaster(suitability_employment,
            "~/GeocomputationProject/Data/Tif/employment_accessibility_england.tif",
            overwrite = TRUE)
