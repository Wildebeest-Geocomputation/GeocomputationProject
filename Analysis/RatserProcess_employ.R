#install.packages("readODS")
library(readODS)
library(sf)
library(dplyr)
library(stringr)
library(tmap)
library(terra)

jts_data <- read_ods(
  path = "C:\\Users\\alfat\\Documents\\STA_MSc\\R\\Group Assignment\\Data\\road_networks\\jts0501.ods",       # Replace with your actual file path
  sheet = "2019_REVISED",     # The specific sheet name
  skip = 10                    # Skips the text before the actual table
)

#Check the data
head(jts_data)

# Select the relevant "Employment 5000+" columns
jts_clean <- jts_data[
  , c("LSOA_code", "5000EmpCart", "5000EmpPTt")
]
summary(jts_clean)
# Count NAs in every column
colSums(is.na(jts_clean))

# Import the shapefile
lsoa_map <- st_read("C:\\Users\\alfat\\Documents\\STA_MSc\\R\\Group Assignment\\Data\\road_networks\\Lower_layer_Super_Output_Areas_December_2021_Boundaries_EW_BSC_V4_-5236167991066794441\\LSOA_2021_EW_BSC_V4.shp")

# Filter to keep only rows where the ID starts with "E"
england_only_map <- lsoa_map %>%
  filter(str_detect(LSOA21CD, "^E"))

nrow(england_only_map)
# Merge
dist_5000 <- left_join(england_only_map, jts_clean, by = c("LSOA21CD" = "LSOA_code"))

# Make a new column that is the average of both distances
dist_5000$`5000EmpAvgt` = (dist_5000$`5000EmpCart`+dist_5000$`5000EmpPTt`)/2

# Create raster grid (1km resolution to match your other maps)
grid_res <- 1000  # 1km
r_grid <- rast(ext(england_only_map), resolution = grid_res, crs = crs(england_only_map))

# Rasterize the hexagonal data
employment_raster <- rasterize(
  vect(dist_5000),
  r_grid,
  field = "5000EmpAvgt",
  fun = "mean"
)

# Mask to England boundary
employment_raster <- mask(employment_raster, vect(england_only_map))

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
     main = "Avergae Distance to Large Employment Centres",
     col = hcl.colors(100, "viridis", rev = FALSE),
     axes = FALSE,
     box = FALSE)


# Save the raster
writeRaster(suitability_employment,
            "C:\\Users\\alfat\\Documents\\STA_MSc\\R\\Group Assignment\\Data\\employment_accessibility_england.tif",
            overwrite = TRUE)
