# https://stackoverflow.com/questions/76153155/creating-a-hexagonal-map-from-a-shapefile-in-r
library("sf")
library("tidyverse")
library("rnaturalearth")
library("rnaturalearthdata")

UK <- ne_countries(scale = "large", country = "United Kingdom", returnclass = "sf")%>%
  st_geometry()%>%
  st_transform(27700)

UK_proj <- st_transform(UK, 27700) # to British National Grid (m)
hexgrid <- st_make_grid(UK, cellsize = 20000, what = 'polygons', square = FALSE)%>%st_as_sf()

hexgrid_UK <- hexgrid[c(unlist(st_contains(UK, hexgrid)), unlist(st_overlaps(UK, hexgrid))) ,]

UK%>%plot()
hexgrid_UK%>%plot(add = TRUE)
