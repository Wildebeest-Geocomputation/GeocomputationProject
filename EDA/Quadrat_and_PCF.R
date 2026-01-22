library(tidyverse)
library(sf)
library(spatstat)

# Read data and convert to sf object
data <- read_csv("./Data/Example/UK_Data_Centers.csv") %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326) %>%
  st_transform(27700)

# Extract coordinates
coords <- st_coordinates(data)

# Get the bounding box for the observation window
bbox <- st_bbox(data)

# Create observation window (owin)
window <- owin(xrange = c(bbox["xmin"], bbox["xmax"]),
               yrange = c(bbox["ymin"], bbox["ymax"]))

# Create point pattern object
ppp_data <- ppp(x = coords[,1],
                y = coords[,2],
                window = window)

# Remove duplicates - keep only unique locations
ppp_data_unique <- unique(ppp_data)

cat("Original points:", ppp_data$n, "\n")
cat("Unique points:", ppp_data_unique$n, "\n")
cat("Removed:", ppp_data$n - ppp_data_unique$n, "duplicates\n")

# Summary of point pattern
print(summary(ppp_data_unique))

# Source - https://stackoverflow.com/a
# Posted by Szymon Cogiel
# Retrieved 2026-01-13, License - CC BY-SA 4.0
q<-quadrat.test(ppp_data_unique, nx = 4, ny = 4)
q
plot(q)
plot(ppp_data_unique, add=TRUE)

# plot
quadrat_counts <- quadratcount(ppp_data_unique, nx = 7, ny = 7)
plot(quadrat_counts, main = "Data Centers per Quadrat")
plot(ppp_data_unique, add = TRUE, pch = 20, cex = 0.5, col = "red")


#_______________________________________________________________________
#Pair correlation function: test for clustering

# Calculate PCF
pcf_result <- pcf(ppp_data_unique)

# Calculate PCF with confidence envelopes (tests against randomness)
pcf_envelope <- envelope(ppp_data_unique,
                         pcf,
                         nsim = 99,           # 99 simulations for CSR
                         verbose = FALSE)

par(mfrow = c(1, 1), mar = c(4, 4, 3, 2))

plot(pcf_envelope,
     main = "Pair Correlation Function\nTest for Clustering vs Randomness",
     xlab = "Distance (meters)",
     ylab = "g(r)",
     lwd = 2,
     col = "blue",
     xlim = c(0, 20000))  # Adjust distance range as needed

# Add randomness line
abline(h = 1, col = "red", lty = 2, lwd = 2)

#RANDOM distribution (CSR) at distance r
#CLUSTERING at distance#
#DISPERSION/INHIBITION at distance#

#Blue line ABOVE grey → Significant CLUSTERING#
#Blue line BELOW grey → Significant DISPERSION#

