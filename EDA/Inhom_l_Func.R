library(spatstat)
library(sf)
library(readr)
source('utils/boundaries.R')

data_centers_sf <- read_csv("~/GeocomputationProject/Data/UK_Data_Centers.csv") %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326) %>%
  st_transform(27700)

# Convert SpatVector to sf, then union
england <- st_union(st_as_sf(england_bng))
england_owin <- as.owin(england)

coords <- st_coordinates(data_centers_sf)
dc_ppp <- ppp(x = coords[,1],
              y = coords[,2],
              window = england_owin)

lambda <- density(dc_ppp, sigma = 70000)

Linhom_result <- Linhom(dc_ppp, lambda = lambda)
plot(Linhom_result, . - r ~ r, main="L-Inhomogeneous Function",xlim = c(0, 30000), xlab="r (distance in metres)")
abline(h = 0, lty = 2)

L_hom <- Lest(dc_ppp)
plot(L_hom, . - r ~ r, main="Homogeneous L-function", xlim = c(0, 30000))
