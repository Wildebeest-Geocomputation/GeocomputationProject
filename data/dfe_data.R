#install.packages("readODS")
library(readODS)
library(sf)
library(dplyr)
library(stringr)
library(tmap)
library(terra)

jts_data <- read_ods(
  path = "C:\\Users\\alfat\\Documents\\STA_MSc\\R\\Group Assignment\\Data\\road_networks\\jts0501.ods",       # Replace with your actual file path
  sheet = "2019_REVISED",     # The specific sheet name
  skip = 10                    # Skips the text before the actual table
)

#Check the data
head(jts_data)

# Select the relevant "Employment 5000+" columns
jts_clean <- jts_data[
  , c("LSOA_code", "5000EmpCart", "5000EmpPTt")
]
summary(jts_clean)
# Count NAs in every column
colSums(is.na(jts_clean))

# Import the shapefile
lsoa_map <- st_read("C:\\Users\\alfat\\Documents\\STA_MSc\\R\\Group Assignment\\Data\\road_networks\\Lower_layer_Super_Output_Areas_December_2021_Boundaries_EW_BSC_V4_-5236167991066794441\\LSOA_2021_EW_BSC_V4.shp")

# Filter to keep only rows where the ID starts with "E"
england_only_map <- lsoa_map %>%
  filter(str_detect(LSOA21CD, "^E"))

nrow(england_only_map)
# Merge
dist_5000 <- left_join(england_only_map, jts_clean, by = c("LSOA21CD" = "LSOA_code"))

# Make a new column that is the average of both distances
dist_5000$`5000EmpAvgt` = (dist_5000$`5000EmpCart`+dist_5000$`5000EmpPTt`)/2

# Create hexagonal grid
hex_grid <- st_make_grid(dist_5000,
                         cellsize = 10000,
                         square = FALSE) %>%
  st_as_sf() %>%
  mutate(hex_id = row_number())

# Spatial join: Assign LSOAs to hexagons and aggregate
hex_data <- st_join(hex_grid, dist_5000) %>%
  group_by(hex_id) %>%
  summarise(mean_avg = mean(`5000EmpAvgt`, na.rm = TRUE),
            n_lsoas = n()) %>%
  filter(!is.na(mean_avg))

# Plot
# tm_shape(dist_5000) + tm_fill(col="5000EmpAvgt", title="Travel Time in Minutes") +
#   tm_layout(main.title="Average Distance to Large Employment Centres",
#             legend.outside=TRUE)

# Plot
tm_shape(hex_data) +
  tm_fill(col = "mean_avg",
          title = "Travel Time (minutes)",
          palette = "Reds",
          style = "fixed",
          breaks = c(0, 20, 40, 60, 80, 100, 120),
          labels = c("0-20", "20-40", "40-60", "60-80", "80-100", "100-120+")
          ) +
  tm_layout(main.title = "Average Distance to Large Employment Centres",
            legend.outside = TRUE)
