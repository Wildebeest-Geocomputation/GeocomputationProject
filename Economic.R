## land value
library(readxl)
library(tidyverse)
# reed first sheet
Residential <- read_xlsx("PData/Individual/landvalue.xlsx", sheet="Residential")%>%filter(!is.na(`...2`))
Industrial <- read_xlsx("PData/Individual/landvalue.xlsx", sheet="Industrial")%>%filter(!is.na(`...3`))
# Crime <- - read_xlsx("PData/Individual/crime.xlsx", sheet="Table P1")
# Industrial%>%filter(!is.na(`...3`))
# Residential%>%filter(!is.na(`...2`))
# rename columns with first row, then remove first row
colnames(Residential) <- Residential[1, ]
Residential <- Residential[-1, ]

colnames(Industrial) <- Industrial[1, ]
Industrial <- Industrial[-1, ]

library(rgeoboundaries)
uk_l2 <- gb_adm2("United Kingdom")
uk_l2

library(osmdata)
library(sf)
library(ggplot2)

place_name <- "Amber Valley"

query <- opq(bbox = place_name) %>%
  add_osm_feature(key = "boundary", value = "administrative") %>%
  add_osm_feature(key = "admin_level", value = "8")

osm_results <- osmdata_sf(query)
county_boundary <- osm_results$osm_multipolygons %>%
  filter(name == "Amber Valley")

county_boundary%>%
  ggplot() +
  geom_sf() +
  ggtitle(paste("Boundary of", place_name))

