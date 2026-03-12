library(sf)
library(tmap)
library(terra)
library(tidyverse)
source('utils/fuzzy.R')
source('utils/boundaries.R')

r_grid <- rast(ext(england_bng), resolution = 1000, crs = crs(england_bng))

# brownfield <- read_csv("./PData/brownfield-site.csv")
brownfield <- read_csv('./MaxEnt_data/brownfield-site.csv')
brownfield_england <- brownfield%>%
  filter(!is.na(point)) %>%
  st_as_sf(wkt = 'point', crs = 4326)%>%
  st_transform(crs = 27700)%>%
  st_filter(st_as_sf(england_bng))


plot(brownfield_england)
england_sf <- st_as_sf(england_bng)
brownfield_sf <- st_as_sf(brownfield_england)
england_sf$brownfield_count <- lengths(st_intersects(england_sf, brownfield_sf))

tm_basemap("CartoDB.Positron") +
  tm_shape(england_sf) +
  tm_polygons("brownfield_count", palette = "Reds", title = "Brownfield Sites Count")

tm_basemap("CartoDB.Positron") +
tm_shape(england_sf) +
  tm_polygons("brownfield_count", palette = "Reds", title = "Brownfield Sites Count") +
  tm_compass(position = c("right", "top")) +
  tm_scale_bar(position = c("right", "bottom")) +
  tm_grid(labels.size = 0.7, n.x = 5, n.y = 5,
          lwd = 0.1,
          alpha = 0.5,
          labels.inside.frame = FALSE,
          projection = 27700)+
  tm_layout(
    main.title.size = 1,
    legend.outside = FALSE,
    legend.position = c("left", "top"),
    legend.bg.color = "white",
    legend.bg.alpha = 0.5,
    legend.frame = TRUE
  )

tmap_mode("plot")
tm_basemap("CartoDB.Positron") +
  tm_shape(brownfield_england) +
  tm_dots(size = 0.01, col = "red")

# raster ize points to raster grid, it will assign value 1 to the cells with points
r_points <- rasterize(vect(brownfield_england), r_grid, field = 1)
dist_grid <- distance(r_points)
plot(dist_grid)
suitability_points <- app(dist_grid, fun = function(x) fuzzy_decrease(x, max_dist = 5000))
suitability_points <- mask(suitability_points, england_bng)

plot(suitability_points)

# testing function from FullProcess
# source('utils/fullpreprocess.R')
# suitability_points <- calculate_distance(brownfield_england, grid_size=1000,
#                                          type='point', save_name='./Data/Tif/brownfield',
#                                          max_dist=5000, suitability_type='decrease')


################## Final part
if(st_crs(data_centers_sf) != st_crs(brownfield_england)) {
  data_centers_sf <- st_transform(data_centers_sf, st_crs(brownfield_england))
}
nearest_index <- st_nearest_feature(data_centers_sf, brownfield_england)

dist_to_brownfield <- st_distance(data_centers_sf, brownfield_england[nearest_index, ], by_element = TRUE)
min_distances_km <- as.numeric(dist_to_brownfield) / 1000
median_dist <- median(min_distances_km, na.rm = TRUE)


plot_data <- data_centers_sf %>%
  mutate(dist_brownfield_km = min_distances_km) %>%
  st_drop_geometry()

med_dist <- median(plot_data$dist_brownfield_km, na.rm = TRUE)

ggplot(plot_data%>%filter(dist_brownfield_km<20), aes(x = dist_brownfield_km)) +
  geom_histogram(binwidth = 1, fill = "lightblue", color = "darkblue", alpha = 0.7) +
  geom_vline(xintercept = med_dist,
             color = "red", linetype = "dashed", linewidth = 1.2) +
  annotate("text", x = med_dist + 0.2, y = Inf,
           label = paste("Median:", round(med_dist, 2), "km"),
           vjust = 1.5, hjust = 0, color = "red", size = 4, fontface = "bold") +

  labs(title = "Distance to Brownfield locations",
       subtitle = "Distribution for UK Data Centers",
       x = "Distance to Brownfield (km)",
       y = "Count") +

  scale_x_continuous(breaks = seq(0, max(plot_data$dist_brownfield_km, na.rm=TRUE) + 2, 2)) +

  theme_minimal() +
  theme(plot.title = element_text(size = 14, face = "bold"))

