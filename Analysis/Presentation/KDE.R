library(raster)
library(adehabitatHR)
library(tidyverse)
library(sf)
library(tmap)
library(leaflet)

uk <- st_read(dsn="./PData/CED_MAY_2025_EN_BFE_1747433965651526042/CED_MAY_2025_EN_BFE.shp", layer="CED_MAY_2025_EN_BFE")
brownfield <- read.csv('./PData/brownfield-site.csv')
greenbelt <- read.csv('./PData/green-belt.csv')
df_all <- bind_rows(brownfield, greenbelt)
df_all$prefix%>%unique()
# plot brownfield$data

# df_all <- brownfield
data_sf <- st_as_sf(df_all, wkt = "point", crs = 4326)

data_sp <- as_Spatial(data_sf)
kde.output <- kernelUD(data_sp, h="href", grid = 1000)
h0 <- kde.output@h$h
kde.output <- kernelUD(data_sp, h=h0*1, grid = 1000)
plot(kde.output)

kde <- raster(kde.output)
# sets projection to British National Grid
# projection(kde) <- CRS("+init=EPSG:27700")

tm_shape(kde) + tm_raster()

kde_trim <- kde
kde_trim[kde_trim <= 0] <- NA

tm_basemap(providers$Esri.WorldTopoMap)+
tm_shape(kde_trim) +
  tm_raster(style = "quantile", n = 7, palette = "Blues", alpha = 0.5,
            legend.format = list(digits = 3, scientific = TRUE)) +
  tm_layout(legend.outside = TRUE, frame = FALSE)
