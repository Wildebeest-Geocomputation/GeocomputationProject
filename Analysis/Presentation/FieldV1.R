library(tmap)
library(leaflet)
library(sp)
library(sf)
library(raster)
library(adehabitatHR)
library(tidyverse)

brownfield <- read.csv('./PData/brownfield-site.csv')
greenbelt <- read.csv('./PData/green-belt.csv')
brownfield

df_all <- bind_rows(brownfield, greenbelt)
df_all$prefix%>%unique()
# plot brownfield$data

data_sf <- st_as_sf(df_all, wkt = "point", crs = 4326)
data_bng <- st_transform(data_sf, 27700)

pal <- colorFactor(palette = "Set1", domain = data_sf$prefix)

# leaflet(data_sf) %>%
#   addProviderTiles(providers$CartoDB.Positron) %>%
#   addCircleMarkers(radius = 4, stroke = FALSE, fillOpacity = 0.5,
#                    popup = ~paste0("<b>", name, "</b><br/>", reference),
#                    fillColor = ~pal(prefix))

# data_bng <- st_transform(df_all, 27700)
coords <- st_coordinates(data_bng)
dat <- st_drop_geometry(data_bng)
dat$id <- factor(dat$typology)

sp_pts <- SpatialPointsDataFrame(
  coords = coords,
  data   = dat["id"],
  proj4string = CRS(SRS_string = "EPSG:27700")
)

ud <- kernelUD(sp_pts["id"], h = "href", grid = 300)
plot(ud$geography)

hr95_sf_wgs <- ud%>%getverticeshr(percent=90)%>%st_as_sf()%>%st_transform(4326)
pts_wgs <- st_transform(data_bng, 4326)

leaflet()%>%
  # addProviderTiles(providers$CartoDB.Positron)%>%
  addProviderTiles(providers$Esri.WorldTopoMap)%>%
    # addPolygons(data = hr95_sf_wgs,  weight = 2, color = "red")%>%
  addCircleMarkers(
    data = pts_wgs, radius = 4, stroke = FALSE, fillOpacity = 0.7,
    clusterOptions = markerClusterOptions()
  )%>%
  addScaleBar(position = "bottomleft")

tmap_mode("view")
tm_basemap(providers$Esri.WorldTopoMap)+
tm_shape(hr95_sf_wgs) +
  # tm_polygons(fill_alpha = 0.4, col = "red") +
  tm_shape(pts_wgs) +
  tm_dots(col = "prefix", size = 0.5, palette = "Set2",
          col.legend = tm_legend(title = "Prefix")) +
  tm_scale_bar() +
  tm_compass() +
  tm_layout(legend.position = c("right", "top")) +
  tm_view(basemaps = providers$CartoDB.Positron)







