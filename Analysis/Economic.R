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

Industrial

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



library(tidyverse)
library(osmdata)
library(sf)

get_la_geometry <- function(la_name, region_name = "") {

  search_query <- paste(la_name, "UK", sep = ", ")
  message(paste("正在查詢:", search_query, "..."))
  # Sys.sleep(1)

  tryCatch({
    q <- opq(bbox = search_query) %>%
      add_osm_feature(key = "boundary", value = "administrative") %>%
      add_osm_feature(key = "admin_level", value = "8")
    osm_results <- osmdata_sf(q)

    if (!is.null(osm_results$osm_multipolygons)) {

      target_poly <- osm_results$osm_multipolygons %>%
        filter(name == la_name)
      if (nrow(target_poly) == 0) {
        return(osm_results$osm_multipolygons$geometry[1])
      } else {
        return(target_poly$geometry[1])
      }

    } else {
      warning(paste("can't find multipolygons:", search_query))
      return(NULL)
    }

  }, error = function(e) {
    warning(paste("failed:", search_query, "-", e$message))
    return(NULL)
  })
}

df_with_geometry <- Industrial %>%
  mutate(geometry = map2(`Local Authority`, Region, get_la_geometry))

df_clean <- df_with_geometry %>%
  filter(!map_lgl(geometry, is.null)) %>%
  mutate(geometry = st_sfc(geometry)) %>%
  st_as_sf()

df_with_geometry%>%
ggplot() +
  geom_sf(aes(fill = `£/ha`)) +
  theme_minimal() +
  ggtitle("Industrial Regions with Geometry")


## https://www.data.gov.uk/dataset/4e39e20e-2be5-4dd1-a205-62aaeb6a86e1/land-cover-map-2024-uk-land-cover-statistics

file_path <- "./PData/0171ccb2-1c0c-404f-b782-e7204a86a92f/data/england_regions_lcm2024_statistics.gpkg"
st_layers(file_path)

lcm_data <- st_read(file_path, layer = "regions")

library(sf)
library(tmap)
library(dplyr)

plot_lcm_tmap <- function(data, class_ids) {
  class_lookup <- c(
    "1" = "Broadleaved woodland (1)",
    "2" = "Coniferous woodland (2)",
    "3" = "Arable & Horticulture (3)",
    "4" = "Improved Grassland (4)",
    "20" = "Urban (20)"
  )

  target_cols <- paste0("percent_", class_ids)

  valid_cols <- target_cols[target_cols %in% names(data)]
  panel_titles <- sapply(class_ids, function(id) {
    str_id <- as.character(id)
    if (str_id %in% names(class_lookup)) {
      return(class_lookup[[str_id]])
    } else {
      return(paste("Class", id))
    }
  })

  tmap_mode("view")

  tm_shape(data) +
    tm_polygons(col = valid_cols,
                style = "jenks",
                palette = "YlGnBu",
                title = "Coverage (%)",
                border.col = "white",
                lwd = 0.5) +
    tm_layout(panel.labels = panel_titles,
              main.title = "LCM2024: Economic Land Use Distribution",
              main.title.size = 1.2,
              legend.outside = TRUE,
              legend.outside.position = "right",
              frame = FALSE)
}

plot_lcm_tmap(lcm_data, 1)

## crime

library(sf)
library(dplyr)
library(purrr)


data_folder <- "./PData/Individual/2025-09"
file_list <- list.files(path = data_folder,
                        pattern = "\\.csv$",
                        full.names = TRUE)

all_data <- file_list %>%
  map(st_read) %>%
  bind_rows()

all_data$Longitude <- as.numeric(as.character(all_data$Longitude))
all_data$Latitude  <- as.numeric(as.character(all_data$Latitude))
clean_data <- all_data %>%
  filter(!is.na(Longitude) & !is.na(Latitude)) %>%
  filter(is.finite(Longitude) & is.finite(Latitude))
crime_data_sf <- st_as_sf(clean_data, coords = c("Longitude", "Latitude"),crs = 4326)

library(leaflet)
tmap_mode("plot")
tm_basemap(providers$Esri.WorldTopoMap)+
  tm_shape(crime_data_sf) +
  tm_dots(col = "Crime.type", border.col = NA, alpha = 1, size = 0.1, palette = "Dark2",
          col.legend = tm_legend(title = "Prefix"))

