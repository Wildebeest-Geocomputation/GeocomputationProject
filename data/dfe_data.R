#install.packages("readODS")
library(readODS)
library(sf)
library(dplyr)

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

# Import the shapefile
lsoa_map <- st_read("C:\\Users\\alfat\\Documents\\STA_MSc\\R\\Group Assignment\\Data\\road_networks\\Lower_layer_Super_Output_Areas_December_2021_Boundaries_EW_BSC_V4_-5236167991066794441\\LSOA_2021_EW_BSC_V4.shp")

# Merge
dist_5000 <- left_join(lsoa_map, jts_clean, by = c("LSOA21CD" = "LSOA_code"))
