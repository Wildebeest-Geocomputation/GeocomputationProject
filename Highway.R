library(sf)
library(dplyr)
library(tmap)
library(stringr)

# Set your working directory to where the shapefiles are
setwd("C:\\Users\\alfat\\Documents\\STA_MSc\\R\\Group Assignment\\Data\\road_networks\\Download_2869127\\open-roads_6194291")

# Read and merge all RoadLink shapefiles at once
# List all the RoadLink.shp files
road_files <- list.files(pattern = "*_RoadLink\\.shp$",
                         full.names = TRUE,
                         recursive = FALSE)

# Check what files were found
print(road_files)

# Read all shapefiles and combine them
all_roads <- do.call(rbind, lapply(road_files, st_read))

# Filter for motorways and primary A-roads
strategic_roads <- all_roads %>%
  filter(class == "Motorway" |
           (class == "A Road" & primary == "true"))
table(strategic_roads$class)

# Now read your England boundary
uk_boundary <- st_read("C:\\Users\\alfat\\Documents\\STA_MSc\\R\\Group Assignment\\Data\\road_networks\\Local_Authority_Districts_May_2024_Boundaries_UK_BGC_-5850961694214429102\\LAD_MAY_2024_UK_BGC.shp")

# Filter for England only (exclude Scotland, Wales, Northern Ireland)
england_boundary <- uk_boundary %>%
  filter(str_detect(LAD24CD, "^E"))
# Clip to England only
england_strategic <- st_intersection(strategic_roads, england_boundary)

# Remove any NA values in class column
england_strategic <- england_strategic %>%
  filter(!is.na(class))

# Create the map
tmap_mode("plot")

tm_shape(england_boundary) +
  tm_borders(col = "grey30", lwd = 1) +
  tm_fill(col = "grey95") +
  tm_shape(england_strategic) +
  tm_lines(col = "class",
           palette = c("Motorway" = "blue", "A Road" = "red"),
           lwd = 1.5,
           title.col = "Road Type") +
  tm_layout(title = "Major Road Network - England",
            frame = FALSE,
            legend.outside = TRUE,
            legend.bg.color = "white",
            legend.bg.alpha = 0.8)
