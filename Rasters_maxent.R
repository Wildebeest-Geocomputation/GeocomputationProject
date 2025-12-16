
# DATA CENTER SUITABILITY ANALYSIS - ENGLAND
# Part 1: GEOLOGY LAYER PROCESSING

# Description: This script processes geology data to create a suitability layer
#              for data center location in England at 1km resolution.
# CRS: EPSG:27700 (British National Grid - BNG)
# Resolution: 1km

library(terra)      # Spatial raster data processing
library(sf)         # Simple features for vector data
library(dplyr)      # Data manipulation
library(viridis)    # Color palettes for visualization

# Load England administrative boundaries
england_boundaries <- st_read("C:/Temp_R/england_ctry_2022.shp")

# Load geology data for England
geology_england <- st_read("C:/Temp_R/geology_england.gpkg")

# PRE-PROCESSING: FILTER SUITABLE GEOLOGY TYPES
# Filter for most suitable soil types for data center foundations
# SAND AND GRAVEL and SAND provide good drainage and stable foundations
geology_best <- geology_england %>%
  filter(ROCK_D %in% c("SAND AND GRAVEL", "SAND"))

cat("  Original features:", nrow(geology_england), "\n")
cat("  Suitable features:", nrow(geology_best), "\n")

# Save filtered geology layer
st_write(geology_best,
         "C:/Temp_R/GeocomputationProject/geology_best_dc.gpkg",
         delete_dsn = TRUE,
         quiet = TRUE)

# Optional: Create scored geology map for visualization
geology_scored <- geology_england %>%
  mutate(
    suitability = case_when(
      ROCK_D == "SAND AND GRAVEL" ~ 3,  # Highest suitability
      ROCK_D == "SAND" ~ 2,             # High suitability
      ROCK_D == "DIAMICTON" ~ 1,        # Medium suitability
      TRUE ~ 0                          # Low/No suitability
    )
  )

# Visualize scored geology
plot(geology_scored["suitability"],
     main = "Geology Suitability Score",
     key.pos = 4)

# CREATE 1KM REFERENCE GRID
# Convert England boundaries to SpatVector (terra format)
england_vect <- vect(england_boundaries)

# Define grid resolution
grid_res <- 1000  # 1km in meters

# Create base raster grid
r_grid <- rast(
  ext(england_vect),      # Extent from England boundaries
  resolution = grid_res,  # 1km resolution
  crs = crs(england_vect) # BNG (EPSG:27700)
)

# Assign unique ID to each cell
values(r_grid) <- 1:ncell(r_grid)

# Mask grid to England boundaries only
r_grid_masked <- mask(r_grid, england_vect)

cat("  Grid dimensions:", ncol(r_grid_masked), "x", nrow(r_grid_masked), "\n")
cat("  Total cells:", ncell(r_grid_masked), "\n")
cat("  CRS:", crs(r_grid_masked), "\n")

# Visualize grid
plot(r_grid_masked,
     main = "1km Grid - England (BNG)",
     axes = FALSE,
     legend = FALSE)

#PROCESS GEOLOGY LAYER FOR MAXENT
# Transform geology to BNG (EPSG:27700)
geology_bng <- st_transform(geology_best, crs = 27700)

# Clip geology to England boundaries
cat("  Clipping to England boundaries...\n")
geology_england_clipped <- st_intersection(geology_bng, england_boundaries)

cat("  Clipped features:", nrow(geology_england_clipped), "\n")

# Rasterize geology polygons to 1km grid
cat("  Rasterizing to 1km grid...\n")
r_geology <- rasterize(
  vect(geology_england_clipped),
  r_grid_masked,
  field = 1
)

# Calculate distance from each cell to suitable geology
cat("  Calculating distance to suitable geology...\n")
dist_geology <- distance(r_geology)

# Normalize distances (invert so near = 1, far = 0)
geology_max <- global(dist_geology, "max", na.rm = TRUE)[[1]]
suitability_geology <- 1 - (dist_geology / geology_max)

# Apply England mask
suitability_geology <- mask(suitability_geology, england_vect)

cat("✓ Geology suitability calculated\n")
cat("  Value range:",
    round(global(suitability_geology, "min", na.rm = TRUE)[[1]], 3), "-",
    round(global(suitability_geology, "max", na.rm = TRUE)[[1]], 3), "\n\n")

# VISUALIZATION
# Plot with viridis color palette
par(mar = c(2, 2, 3, 5))

plot(suitability_geology,
     main = "Geology Suitability for Data Centers",
     col = viridis(100),
     axes = FALSE,
     legend = TRUE,
     range = c(0, 1),
     plg = list(
       title = "Suitability",
       cex = 0.9
     )
)

# Add England boundary
plot(st_geometry(england_boundaries),
     add = TRUE,
     border = "cyan",
     lwd = 1.5)

# SAVE OUTPUT
# Create output directory
dir.create("C:/Temp_R/Data/Tif", showWarnings = FALSE, recursive = TRUE)

# Save geology suitability raster
writeRaster(suitability_geology,
            "C:/Temp_R/Data/Tif/geology_suitability_1km.tif",
            overwrite = TRUE,
            datatype = "FLT4S")

# Save reference grid
writeRaster(r_grid_masked,
            "C:/Temp_R/Data/Tif/england_grid_1km.tif",
            overwrite = TRUE)

# SUMMARY

cat("OUTPUT FILES:\n")
cat("  • geology_suitability_1km.tif (suitability layer 0-1)\n")
cat("  • england_grid_1km.tif (reference grid)\n")
cat("  • geology_best_dc.gpkg (filtered geology vector)\n\n")

cat("CHARACTERISTICS:\n")
cat("  • Resolution: 1 km (1000 meters)\n")
cat("  • CRS: EPSG:27700 (British National Grid)\n")
cat(paste("  • Dimensions:", nrow(suitability_geology), "x",
          ncol(suitability_geology), "cells\n"))
cat("  • Values: 0 (far from suitable geology) to 1 (on suitable geology)\n\n")

cat("SUITABLE GEOLOGY TYPES:\n")
cat("  • SAND AND GRAVEL (optimal drainage and stability)\n")
cat("  • SAND (good drainage and stability)\n\n")



#Part 2: FLOOD RISK LAYER PROCESSING
# Description: This script processes flood risk data to create a suitability
#              layer for data center location in England at 1km resolution.
#              Areas far from flood zones receive higher suitability scores.
# CRS: EPSG:27700 (British National Grid - BNG)
# Resolution: 1km

library(terra)      # Spatial raster data processing
library(sf)         # Simple features for vector data
library(dplyr)      # Data manipulation
library(viridis)    # Color palettes for visualization

# Load England administrative boundaries
england_boundaries <- st_read("C:/Temp_R/england_ctry_2022.shp")

# Load flood risk areas
flood_risk <- st_read("C:/Temp_R/Flood_Risk_Areas.shp/Flood_Risk_Areas.shp")

# Load reference grid (created in geology processing script)
r_grid_masked <- rast("C:/Temp_R/Data/Tif/england_grid_1km.tif")

cat("  Flood risk features:", nrow(flood_risk), "\n\n")

# VERIFY AND TRANSFORM CRS
# Transform flood risk to BNG (EPSG:27700)
flood_bng <- st_transform(flood_risk, crs = 27700)

# Verify CRS
cat("  Flood risk CRS:", st_crs(flood_bng)$input, "\n")

# CLIP FLOOD RISK TO ENGLAND BOUNDARIES

# Intersect flood risk with England boundaries
flood_england <- st_intersection(flood_bng, england_boundaries)

cat("  Original features:", nrow(flood_bng), "\n")
cat("  Clipped features:", nrow(flood_england), "\n")

# RASTERIZE FLOOD RISK AREAS
# Convert England boundaries to SpatVector
england_vect <- vect(england_boundaries)

# Rasterize flood risk polygons to grid
# Cells with flood risk = 1, cells without = NA
r_flood <- rasterize(
  vect(flood_england),
  r_grid_masked,
  field = 1
)

# Visualize flood risk raster
plot(r_flood,
     main = "Flood Risk Areas (Binary)",
     col = c("lightblue", "red"),
     legend = TRUE)

#CALCULATE DISTANCE TO FLOOD ZONES

# Calculate Euclidean distance from each cell to nearest flood zone
dist_flood <- distance(r_flood)

# Summary statistics
dist_summary <- global(dist_flood, c("min", "max", "mean"), na.rm = TRUE)
cat("\nDistance Statistics (meters):\n")
cat("  Min:", round(dist_summary[1,1], 2), "\n")
cat("  Max:", round(dist_summary[1,2], 2), "\n")
cat("  Mean:", round(dist_summary[1,3], 2), "\n\n")

# Visualize distance raster
plot(dist_flood,
     main = "Distance to Flood Risk Areas (meters)",
     col = viridis(100))

# NORMALIZE TO SUITABILITY SCORES (0-1)

# Normalize distances to 0-1 scale
# 0 = on/near flood zone (unsuitable)
# 1 = far from flood zone (suitable)
flood_max <- global(dist_flood, "max", na.rm = TRUE)[[1]]
suitability_flood <- dist_flood / flood_max

# Apply England mask
suitability_flood <- mask(suitability_flood, england_vect)

cat("  Value range:",
    round(global(suitability_flood, "min", na.rm = TRUE)[[1]], 3), "-",
    round(global(suitability_flood, "max", na.rm = TRUE)[[1]], 3), "\n\n")

# VISUALIZATION WITH CUSTOM STYLE

cat("Creating visualization...\n")

# Custom plotting function with viridis palette
plot_suitability <- function(raster_data, title, england_boundary) {

  # Set margins
  par(mar = c(2, 2, 3, 5))

  # Plot raster with viridis color palette
  plot(raster_data,
       main = title,
       col = viridis(100),  # Viridis palette (purple to yellow)
       axes = FALSE,
       box = FALSE,
       legend = TRUE,
       range = c(0, 1),     # Fixed range 0-1
       plg = list(          # Legend configuration
         title = "Suitability",
         title.cex = 0.9,
         cex = 0.8
       ),
       pax = list(          # Axis configuration
         cex.axis = 0.8
       )
  )

  # Add England boundary outline
  plot(st_geometry(england_boundary),
       add = TRUE,
       border = "cyan",    # Cyan border
       lwd = 1.5)
}

# Create plot
plot_suitability(
  suitability_flood,
  "Flood Risk Suitability for Data Centers",
  england_boundaries
)
#SAVE OUTPUT

# Create output directory
dir.create("C:/Temp_R/Data/Tif", showWarnings = FALSE, recursive = TRUE)

# Save flood suitability raster
writeRaster(suitability_flood,
            "C:/Temp_R/Data/Tif/flood_suitability_1km.tif",
            overwrite = TRUE,
            datatype = "FLT4S")

# Optional: Save high-resolution plot
png("C:/Temp_R/Plots/flood_suitability.png",
    width = 2400, height = 2400, res = 300)
plot_suitability(
  suitability_flood,
  "Flood Risk Suitability for Data Centers",
  england_boundaries
)
dev.off()

#SUMMARY
cat("OUTPUT FILES:\n")
cat("  • flood_suitability_1km.tif (suitability layer 0-1)\n")
cat("  • flood_suitability.png (visualization)\n\n")

cat("CHARACTERISTICS:\n")
cat("  • Resolution: 1 km (1000 meters)\n")
cat("  • CRS: EPSG:27700 (British National Grid)\n")
cat(paste("  • Dimensions:", nrow(suitability_flood), "x",
          ncol(suitability_flood), "cells\n"))
cat("  • Values: 0 (on/near flood zone) to 1 (far from flood zone)\n\n")

cat("INTERPRETATION:\n")
cat("  • High values (0.8-1.0): Safe from flooding - SUITABLE\n")
cat("  • Medium values (0.4-0.8): Moderate distance from flooding\n")
cat("  • Low values (0.0-0.4): Near flood zones - UNSUITABLE\n\n")

cat("METHOD:\n")
cat("  1. Flood risk areas identified from source data\n")
cat("  2. Euclidean distance calculated from each cell to nearest flood zone\n")
cat("  3. Distances normalized to 0-1 scale\n")
cat("  4. Greater distance = higher suitability for data centers\n\n")

# Part 3: SOLAR IRRADIATION LAYER PROCESSING
# Description: This script processes solar irradiation data to create a
#              suitability layer for data center location in England at 1km
#              resolution. Higher solar irradiation enables better renewable
#              energy potential for data center operations.
# CRS: EPSG:27700 (British National Grid - BNG)
# Resolution: 1km

library(terra)      # Spatial raster data processing
library(sf)         # Simple features for vector data
library(dplyr)      # Data manipulation
library(viridis)    # Color palettes for visualization

#LOAD INPUT DATA
# Load England administrative boundaries
england_boundaries <- st_read("C:/Temp_R/england_ctry_2022.shp")

# Load solar irradiation raster (original CRS and resolution)
solar_irradiation <- rast("C:/Temp_R/solar_irradiation_england.tif")

# Load reference grid (created in geology processing script)
r_grid_masked <- rast("C:/Temp_R/Data/Tif/england_grid_1km.tif")

cat("  Original solar CRS:", crs(solar_irradiation, describe = TRUE)$code, "\n")
cat("  Original dimensions:", nrow(solar_irradiation), "x",
    ncol(solar_irradiation), "cells\n\n")

#PREPARE ENGLAND BOUNDARY
# Transform to BNG (EPSG:27700)
england_bng <- st_transform(england_boundaries, crs = 27700)

cat("  Rows in boundary:", nrow(england_bng), "\n")

# Merge geometries if multiple features exist
if(nrow(england_bng) > 1) {
  cat("  Merging", nrow(england_bng), "geometries...\n")
  england_bng <- england_bng %>%
    st_union() %>%
    st_as_sf()
}

# Repair geometry if needed
if(!all(st_is_valid(england_bng))) {
  cat("  Repairing invalid geometries...\n")
  england_bng <- st_make_valid(england_bng)
}

cat("✓ England boundary prepared\n")
cat("  CRS:", st_crs(england_bng)$input, "\n")
cat("  Bounding box:\n")
print(st_bbox(england_bng))
cat("\n")

# Convert to SpatVector (terra format)
england_vect <- vect(england_bng)

# REPROJECT SOLAR IRRADIATION TO BNG
# Reproject to British National Grid (EPSG:27700)
solar_bng <- project(solar_irradiation, "EPSG:27700")

cat("  New CRS:", crs(solar_bng, describe = TRUE)$code, "\n")
cat("  Dimensions after reprojection:", nrow(solar_bng), "x",
    ncol(solar_bng), "cells\n\n")

# RESAMPLE TO 1KM RESOLUTION
# Resample to match 1km reference grid
# Method: bilinear (appropriate for continuous data)
solar_1km <- resample(solar_bng, r_grid_masked, method = "bilinear")

cat("  New dimensions:", nrow(solar_1km), "x", ncol(solar_1km), "cells\n")
cat("  Resolution:", res(solar_1km)[1], "x", res(solar_1km)[2], "meters\n\n")

# MASK TO ENGLAND BOUNDARIES
# Apply England mask to keep only cells within England
solar_england <- mask(solar_1km, england_vect)

# Visualize masked solar irradiation
plot(solar_england,
     main = "Solar Irradiation - England (1km)",
     col = heat.colors(100))

# NORMALIZE TO SUITABILITY SCORES (0-1)

# Get min and max values
solar_min <- global(solar_england, "min", na.rm = TRUE)[[1]]
solar_max <- global(solar_england, "max", na.rm = TRUE)[[1]]

cat("  Original value range:\n")
cat("    Min:", round(solar_min, 2), "\n")
cat("    Max:", round(solar_max, 2), "\n")

# Normalize to 0-1 scale
# 0 = lowest solar irradiation (less suitable)
# 1 = highest solar irradiation (most suitable)
suitability_solar <- (solar_england - solar_min) / (solar_max - solar_min)

# Verify normalization
norm_summary <- global(suitability_solar, c("min", "max", "mean"), na.rm = TRUE)
cat("Normalized Statistics:\n")
cat("  Min:", round(norm_summary[1,1], 4), "\n")
cat("  Max:", round(norm_summary[1,2], 4), "\n")
cat("  Mean:", round(norm_summary[1,3], 4), "\n\n")

# VISUALIZATION WITH CUSTOM STYLE
# Custom plotting function with viridis palette
plot_suitability <- function(raster_data, title, england_boundary) {

  # Set margins
  par(mar = c(2, 2, 3, 5))

  # Plot raster with viridis color palette
  plot(raster_data,
       main = title,
       col = viridis(100),  # Viridis palette (purple to yellow)
       axes = FALSE,
       box = FALSE,
       legend = TRUE,
       range = c(0, 1),     # Fixed range 0-1
       plg = list(          # Legend configuration
         title = "Suitability",
         title.cex = 0.9,
         cex = 0.8
       ),
       pax = list(          # Axis configuration
         cex.axis = 0.8
       )
  )

  # Add England boundary outline
  plot(st_geometry(england_boundary),
       add = TRUE,
       border = "cyan",    # Cyan border
       lwd = 1.5)
}

# Create plot
plot_suitability(
  suitability_solar,
  "Solar Irradiation Suitability for Data Centers",
  england_bng
)

cat("✓ Visualization created\n\n")

#SAVE OUTPUT
# Create output directory
dir.create("C:/Temp_R/Data/Tif", showWarnings = FALSE, recursive = TRUE)

# Save solar suitability raster
writeRaster(suitability_solar,
            "C:/Temp_R/Data/Tif/solar_suitability_1km.tif",
            overwrite = TRUE,
            datatype = "FLT4S")

cat("✓ Output saved: solar_suitability_1km.tif\n\n")

# Optional: Save high-resolution plot
dir.create("C:/Temp_R/Plots", showWarnings = FALSE)

png("C:/Temp_R/Plots/solar_suitability.png",
    width = 2400, height = 2400, res = 300)
plot_suitability(
  suitability_solar,
  "Solar Irradiation Suitability for Data Centers",
  england_bng
)
dev.off()

cat("✓ Plot saved: solar_suitability.png\n\n")

# Save England boundary for future use
st_write(england_bng,
         "C:/Temp_R/england_bng.gpkg",
         delete_dsn = TRUE,
         quiet = TRUE)

# SUMMARY
cat("OUTPUT FILES:\n")
cat("  • solar_suitability_1km.tif (suitability layer 0-1)\n")
cat("  • solar_suitability.png (visualization)\n")
cat("  • england_bng.gpkg (England boundary reference)\n\n")

cat("CHARACTERISTICS:\n")
cat("  • Resolution: 1 km (1000 meters)\n")
cat("  • CRS: EPSG:27700 (British National Grid)\n")
cat(paste("  • Dimensions:", nrow(suitability_solar), "x",
          ncol(suitability_solar), "cells\n"))
cat("  • Values: 0 (low solar) to 1 (high solar irradiation)\n\n")

cat("INTERPRETATION:\n")
cat("  • High values (0.8-1.0): Excellent solar potential - HIGHLY SUITABLE\n")
cat("  • Medium values (0.4-0.8): Moderate solar potential\n")
cat("  • Low values (0.0-0.4): Poor solar potential - LESS SUITABLE\n\n")

cat("PROCESSING STEPS:\n")
cat("  1. Original solar irradiation raster loaded\n")
cat("  2. Reprojected to BNG (EPSG:27700)\n")
cat("  3. Resampled to 1km resolution using bilinear interpolation\n")
cat("  4. Masked to England boundaries\n")
cat("  5. Normalized to 0-1 scale for suitability\n\n")

cat("RENEWABLE ENERGY CONTEXT:\n")
cat("  Data centers with high solar suitability can:\n")
cat("  • Offset energy consumption with on-site solar generation\n")
cat("  • Reduce carbon footprint and operational costs\n")
cat("  • Meet sustainability and green energy targets\n\n")
