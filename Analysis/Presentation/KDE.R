library(raster)
library(adehabitatHR)
library(tidyverse)
library(sf)
library(tmap)
library(leaflet)
source('utils/boundaries.R')

# uk <- st_read(dsn="./PData/CED_MAY_2025_EN_BFE_1747433965651526042/CED_MAY_2025_EN_BFE.shp", layer="CED_MAY_2025_EN_BFE")
# brownfield <- read.csv('./PData/brownfield-site.csv')
# greenbelt <- read.csv('./PData/green-belt.csv')
# df_all <- bind_rows(brownfield, greenbelt)
# df_all$prefix%>%unique()
# df_all <- brownfield
# data_sf <- st_as_sf(df_all, wkt = "point", crs = 4326)


data_sf_bng <- read_csv("Data/Example/UK_Data_Centers.csv") %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326) %>%
  st_transform(27700)

england_sf_bng <- st_as_sf(england_bng)
intersect_matrix <- st_intersects(data_sf_bng, england_sf_bng, sparse = FALSE)
data_england_sf <- data_sf_bng[apply(intersect_matrix, 1, any), ]
data_england_sf <- st_filter(data_sf_bng, england_sf_bng)

data_sp <- as_Spatial(data_england_sf)
kde.output <- kernelUD(data_sp, h="href", grid = 2000)
h0 <- kde.output@h$h
kde.output <- kernelUD(data_sp, h=h0*0.5, grid = 2000)
plot(kde.output)

kde <- raster(kde.output)
kde[kde <= 0] <- NA

tmap_mode("plot")
datacentre_tmap <- tm_basemap(providers$Esri.WorldTopoMap) +
  tm_shape(kde, bbox = st_as_sf(england_bng)) +
  tm_raster(style = "quantile", n = 10, palette = "Blues", alpha = 0.6, title = "KDE value",
            legend.format = list(scientific = TRUE)) +
tm_compass(position = c("right", "top")) +
  tm_scale_bar(position = c("right", "top")) +
  tm_grid(labels.size = 0.7, n.x = 5, n.y = 5,
          lwd = 0.1,
          alpha = 0.7,
          labels.inside.frame = FALSE)+
  tm_layout(
    main.title.size = 1,
    legend.outside = FALSE,
    legend.position = c("left", "top"),
    legend.bg.color = "white",
    legend.bg.alpha = 0.7,
    legend.frame = TRUE
  )
tmap_save(datacentre_tmap, filename = "./Data/Layout/datacentre_kde.png", width = 8, height = 10)

tm_basemap(providers$Esri.WorldTopoMap) +
  tm_shape(data_england_sf) +
  tm_dots() +
  tm_compass(position = c("right", "top")) +
  tm_scale_bar(position = c("right", "top")) +
  tm_grid(labels.size = 0.7, n.x = 5, n.y = 5,
          lwd = 0.1,
          alpha = 0.7,
          labels.inside.frame = FALSE)+
  tm_layout(
    main.title.size = 1,
    legend.outside = FALSE,
    legend.position = c("left", "top"),
    legend.bg.color = "white",
    legend.bg.alpha = 0.7,
    legend.frame = TRUE
  )

leaflet(data_england_sf %>% st_transform(4326)) %>%
  addProviderTiles(providers$Esri.WorldTopoMap) %>%
  addCircleMarkers(
    radius = 4,
    stroke = FALSE,
    fillOpacity = 0.7,
    color = "blue",
    clusterOptions = markerClusterOptions(),
    popup = ~Name
  ) %>%
  addScaleBar(position = "bottomleft")

library(spatstat)

window <- as.owin(st_geometry(england_sf_bng))
pts <- st_coordinates(data_england_sf)
p <- ppp(pts[,1], pts[,2], window = window)

dens_ppl <- density(p, sigma = bw.ppl)
plot(dens_ppl, main = "Data center KDE")

library(tmap)
library(stars)
library(sf)

dens_stars <- st_as_stars(dens_ppl)
st_crs(dens_stars) <- st_crs(england_sf_bng)

map_kde <- tm_basemap("CartoDB.Positron") + tm_shape(dens_stars) +
  tm_raster(palette = "YlOrRd",
            title = "Density",
            style = "cont",
            # opacity
            alpha = 0.7
            ) +
  tm_shape(england_sf_bng) +
  tm_borders(col = "black", lwd = 0.5) +
  tm_layout(
    main.title = "Kernel Density Estimation",
    main.title.size = 1,
    legend.outside = FALSE,
    legend.position = c("left", "top"),
    legend.bg.color = "white",
    legend.bg.alpha = 0.6,
    legend.frame = TRUE
  ) +
  tm_compass(position = c("right", "top")) +
  tm_scale_bar(position = c("right", "bottom"))

map_pts <- tm_basemap("CartoDB.Positron") + tm_shape(england_sf_bng) +
  tm_borders(col = "black", lwd = 0.5) +
  tm_shape(data_england_sf) +
  tm_dots(col = "blue",
          size = 0.05,
          alpha = 0.6,
          title = "Data Centers") +
  tm_layout(main.title = "Original Point Data",
            main.title.size = 1,
            legend.outside = FALSE,
            legend.bg.alpha = 0) +
  tm_compass(position = c("right", "top")) +
  tm_scale_bar(position = c("right", "bottom"))

tmap_arrange(map_kde, map_pts, ncol = 2)


# dens_diggle <- density(p, sigma = bw.diggle)
# plot(dens_diggle, main = "Diggle Cross-validation (bw.diggle)")
#
# dens_scott <- density(p, sigma = bw.scott)
# plot(dens_scott, main = "Scott's Rule (bw.scott)")
#
# library(magick)
# sigma_values <- seq(5000, 100000, by = 5000)
# img_list <- image_graph(width = 800, height = 800, res = 96)
#
# for (s in sigma_values) {
#   dens <- density(p, sigma = s)
#   plot(dens, main = paste("Data Center KDE | Sigma =", s, "m"))
# }
# dev.off()
#
# animation <- image_animate(img_list, fps = 1)
# image_write(animation, "datacenter_density.gif")
