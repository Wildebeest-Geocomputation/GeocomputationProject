library(terra)
library(spatstat)
library(sf)

# LOAD RASTERS (To get the BNG Map Window)
tif_files <- list.files(path = "~/GeocomputationProject/Data/Tif", pattern = "\\.tif$", full.names = TRUE)
print(tif_files)
selected_files <- tif_files[c(3,7)]
print(selected_files)
env_stack <- rast(selected_files)
print(names(env_stack))
# rename files for plot
 names(env_stack) <- c("Distance_to_Large_Employers", "Disance_to_Major_and_Strat_Roads")

# Define the Window based on the Rasters (which are already in BNG)
ex <- ext(env_stack)
analysis_window <- owin(xrange = c(ex[1], ex[2]), yrange = c(ex[3], ex[4]))


# LOAD CSV & TRANSFORM TO BNG
dc_df <- read.csv("./Data/Example/UK_Data_Centers.csv")

# Convert to an 'sf' object
dc_sf <- st_as_sf(dc_df, coords = c("lon", "lat"), crs = 4326)

# Transform to British National Grid (Code 27700)
dc_bng <- st_transform(dc_sf, crs = 27700)

# Extract the new BNG coordinates
coords <- st_coordinates(dc_bng)

# Create the Point Pattern Object
dc_ppp <- ppp(x = coords[,1],
              y = coords[,2],
              window = analysis_window)
print(dc_ppp)

# HELPER FUNCTION (For Memory Efficiency - thx AI)
terra_to_im <- function(r) {
  ext_r <- ext(r)
  # Extract values as matrix (fast)
  mat <- as.matrix(r, wide = TRUE)
  # Flip rows for spatstat standard
  mat <- mat[nrow(mat):1, ]
  # Create image
  im(mat, xrange = c(ext_r[1], ext_r[2]), yrange = c(ext_r[3], ext_r[4]))
}


# Analysis loop
par(mfrow = c(2, 1), mar = c(4, 4, 3, 1))

for(i in 1:nlyr(env_stack)) {

  # Get Raster
  current_raster <- env_stack[[i]]

  # Convert to Image
  current_im <- terra_to_im(current_raster)

  # Run Rhohat
  rho_result <- rhohat(dc_ppp, current_im)

  # Plot
  plot(rho_result,
       main = names(env_stack)[i],
       xlab = "Proximity (1 = closest, 0 = farthest)",
       ylab = "Data Center Intensity (λ per m²)",
       legend = FALSE,
       lwd = 2,
       col = "blue")

  # Cleanup
  rm(current_raster, current_im, rho_result)
  gc()
}

par(mfrow = c(1, 1))

