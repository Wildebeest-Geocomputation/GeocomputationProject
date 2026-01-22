library(spatstat)
library(sf)
library(readr)
source('utils/boundaries.R')

data_centers_sf <- read_csv("~/GeocomputationProject/Data/Example/UK_Data_Centers.csv") %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326) %>%
  st_transform(27700)

# Convert SpatVector to sf, then union
england <- st_union(st_as_sf(england_bng))
england_owin <- as.owin(england)

coords <- st_coordinates(data_centers_sf)
dc_ppp <- ppp(x = coords[,1],
              y = coords[,2],
              window = england_owin)

lambda <- density(dc_ppp, sigma = 100000)

Linhom_result <- Linhom(dc_ppp, lambda = lambda)
plot(Linhom_result, . - r ~ r, main="L-Inhomogeneous Function",xlim = c(0, 30000))
abline(h = 0, lty = 2)

L_hom <- Lest(dc_ppp)
plot(L_hom, . - r ~ r, main="Homogeneous L-function", xlim = c(0, 30000))

# KDE for data centres

# Calculate optimal sigmas
sigma_diggle <- bw.diggle(dc_ppp)
sigma_ppl    <- bw.ppl(dc_ppp)
sigma_scott  <- bw.scott(dc_ppp)

# Print values (these will be in meters)
print(paste("Diggle:", sigma_diggle))
print(paste("PPL:", sigma_ppl))

# Plot them to compare
par(mfrow=c(1,2))
plot(dc_ppp)
plot(density(dc_ppp, sigma=sigma_diggle), main="Diggle (Focus on Clusters)")
plot(density(dc_ppp, sigma=sigma_ppl), dimyx=c(512, 512), main="PPL (Likelihood)")
plot(density(dc_ppp, sigma=sigma_scott), main="Scott (Oversmoothed)")

plot(density(dc_ppp, sigma=13052), main="Kernel Density Estimation (KDE) Plot")
