library(sf)
library(readxl)
library(tidyverse)
library(osmdata)
library(rgeoboundaries)
library(leaflet)
library(tmap)

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


uk_l2 <- gb_adm2("United Kingdom")


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

get_la_geometry <- function(la_name, region_name = "") {

  search_query <- paste(la_name, "UK", sep = ", ")
  message(paste("Searching:", search_query, "..."))
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
library(dplyr)
library(purrr)


data_folder <- "./PData/Individual/Crime"
file_list <- list.files(path = data_folder,
                        pattern = "\\.csv$",
                        full.names = TRUE)

all_data <- file_list %>%
  map(st_read) %>%
  bind_rows()
# write.csv(all_data, file = "./PData/Individual/all_crime_data_2025_09.csv", row.names = FALSE)
all_data <- read_csv("./PData/Individual/all_crime_data_2025_09.csv")

all_data$Longitude <- as.numeric(as.character(all_data$Longitude))
all_data$Latitude  <- as.numeric(as.character(all_data$Latitude))
clean_data <- all_data %>%
  filter(!is.na(Longitude) & !is.na(Latitude)) %>%
  filter(is.finite(Longitude) & is.finite(Latitude))
crime_data_sf <- st_as_sf(clean_data, coords = c("Longitude", "Latitude"),crs = 4326)

tmap_mode("plot")
crime_data_sf%>%
  filter(Crime.type == 'Drugs')%>%
tm_shape() +
tm_basemap(providers$Esri.WorldTopoMap)+
  tm_dots(col = "Crime.type", border.col = NA, alpha = 1, size = 0.1, palette = "Dark2",
          col.legend = tm_legend(title = "Prefix"))

## join
stock_of_properties <- read_csv('./PData/Individual/Land/SOP_SCAT_LA_rv_500000_plus.csv')
lsoa_shp <- st_read("./PData/Individual/Local_Authority_Districts_December_2023_Boundaries_UK_BGC_-6607102865052560878/LAD_DEC_2023_UK_BGC.shp")
stock_of_properties
full_sop <- stock_of_properties %>%
  left_join(lsoa_shp, by = c("area_code" = "LAD23CD"))

# feature <- `Computer Centres (Purpose Built)`
feature <- 'Large Distribution Warehouses'
filter_sop <- full_sop%>%
  select(area_name, geometry, value = all_of(feature))%>%
  filter(!st_is_empty(geometry))%>%
  st_as_sf()
filter_sop <- filter_sop %>%
  mutate(value = case_when(
    value == '[c]' ~ '0',
    is.na(value)  ~ '0',
    TRUE ~ value
  ))
filter_sop%>%
  tm_shape() +
  tm_basemap(providers$Esri.WorldTopoMap)+
  tm_polygons(col = 'value', border.col = NA, alpha = 0.7, palette = "YlGnBu")

filter_sop$value%>%unique()

## employment
employment_data <- read_csv('./PData/Individual/employment.csv')
area <- st_read('./PData/Regions_December_2019_General_Clipped_Boundaries_EN_2022_-3961744608053840024/Regions_December_2019_General_Clipped_Boundaries_EN.shp')
map_data <- area %>%
  left_join(employment_data, by = c("rgn19cd" = "ONS Code"))

tmap_mode("view")
tm_shape(map_data) +
  tm_basemap(providers$Esri.WorldTopoMap) +
  tm_polygons(col = 'Employment Rate (%)',
              border.col = "white",
              alpha = 0.7,
              palette = "YlGnBu",
              title = "Employment rate (%)")

# Economic for model
source('utils/boundaries.R')

# This is from Economic.R
# Large Distribution Warehouses
plot(filter_sop)

ldw_england <- filter_sop%>%
  st_as_sf(wkt = 'geometry', crs = 4326)%>%
  st_transform(crs = 27700)%>%
  st_filter(st_as_sf(england_bng))

source('Analysis/FullPreprocess.R')
suitability_points <- calculate_distance(ldw_england, grid_size=1000, type='area', max_dist=5000, suitability_type='increase')



