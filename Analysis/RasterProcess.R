if (!require("geodata")) install.packages("geodata")
if (!require("terra")) install.packages("terra")

library(terra)
library(geodata)

template_raster <- elevation_30s(country="GBR", path=tempdir())

uk_admin_level1 <- gadm(country="GBR", level=1, path=tempdir()) # main boundaries
uk_admin_level2 <- gadm(country="GBR", level=2, path=tempdir()) # more detailed boundaries

# Convert polygons to lines to simulate roads or power lines
power_lines <- as.lines(uk_admin_level1)
roads <- as.lines(uk_admin_level2)
# To speed up computation, crop to a small area around London
crop_extent <- ext(-0.5, 0.5, 51.3, 51.7)
template_raster <- crop(template_raster, crop_extent)
power_lines <- crop(power_lines, crop_extent)
roads <- crop(roads, crop_extent)
# Create Euclidean Distance Raster for raster
plot(template_raster, main = "Elevation")
lines(power_lines, col="red", lwd=2)
lines(roads, col="blue", lwd=1)
# Power Lines
dist_power <- distance(template_raster, power_lines)
# Define Fuzzy Function (Monotonically Decreasing)
fuzzy_decrease <- function(x, max_dist) {
  val <- 1 - (x / max_dist)
  val[val < 0] <- 0
  return(val)
}
# Set maximum distance to 10km (10000m) because this is in geographic coordinates, terra will handle great-circle distances automatically
# Apply function to raster values
suitability_power <- app(dist_power, fun = function(x) fuzzy_decrease(x, max_dist = 10000))

plot(suitability_power, main = "Fuzzy Suitability: Power Lines (Closer is Better)")

# Create Euclidean Distance Raster
dist_roads <- distance(template_raster, roads)

# Define Fuzzy Function (Symmetric / Trapezoidal)
fuzzy_symmetric <- function(x, a, b, c, d) {
  y <- rep(0, length(x))

  # Rising limb (a to b)
  idx_rise <- x > a & x < b
  y[idx_rise] <- (x[idx_rise] - a) / (b - a)

  # Plateau (b to c) - The "Sweet Spot"
  idx_flat <- x >= b & x <= c
  y[idx_flat] <- 1

  # Falling limb (c to d)
  idx_fall <- x > c & x < d
  y[idx_fall] <- 1 - (x[idx_fall] - c) / (d - c)

  return(y)
}

# Here we adjust parameters to fit the example scale: 500m to 2000m is the ideal distance
suitability_roads <- app(dist_roads, fun = function(x) fuzzy_symmetric(x, a=0, b=500, c=2000, d=5000))

plot(suitability_roads, main = "Fuzzy Suitability: Major Roads (Symmetric)")

