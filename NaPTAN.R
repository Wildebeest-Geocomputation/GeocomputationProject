library(tidyverse)
library(sf)
library(tmap)

naptan <- read.csv("C:\\Users\\alfat\\Documents\\STA_MSc\\R\\Group Assignment\\Data\\road_networks\\NaPTAN.csv")

# Filter for active stops if needed
#naptan <- naptan[naptan$Status == "active"]

# We use Easting/Northing (EPSG: 27700)
stops_sf <- st_as_sf(naptan, coords = c("Easting", "Northing"), crs = 27700)

# Read your LA shapefile
la_uk <- st_read("C:\\Users\\alfat\\Documents\\STA_MSc\\R\\Group Assignment\\Data\\road_networks\\Local_Authority_Districts_May_2024_Boundaries_UK_BGC_-5850961694214429102\\LAD_MAY_2024_UK_BGC.shp")

# Filter to keep only rows where the ID starts with "E"
la_england <- la_uk %>%
  filter(str_detect(LAD24CD, "^E"))

#la_england <- st_transform(la_england, crs = 27700)

# Create the map matching your aesthetic
tm_shape(la_england) +
  tm_borders(col = "grey70", lwd = 0.5) +
  tm_fill(col = "white") +
  tm_shape(stops_sf) +
  tm_dots(col = "red",           # or color by transport type
          size = 0.02,
          alpha = 0.6) +
  tm_layout(title = "NaPTAN Transport Locations - England",
            frame = FALSE,
            legend.outside = TRUE,
            bg.color = "white")
