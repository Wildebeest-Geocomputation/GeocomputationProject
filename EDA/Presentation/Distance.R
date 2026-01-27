library(tidyverse)
library(geosphere)
# https://stackoverflow.com/questions/32363998/function-to-calculate-geospatial-distance-between-two-points-lat-long-using-r

# To do:
# distance from DCs (point) to linestring (ex. roadnetwork)
# distance from DCs (point) to polygon (ex. service area, might need to convert to centroid first)
# DCs in a specific area (ex. whithin which land value)

data_centers <- read_csv("data/UK_Data_Centers.csv")
data_centers

distm(data_centers[1, c("lon", "lat")], data_centers[2, c("lon", "lat")], fun = distHaversine)

dist_matrix <- distm(data_centers[, c("lon", "lat")], fun = distHaversine) # unit is meters

max(dist_matrix)
min(dist_matrix[dist_matrix != 0])
dist_matrix%>%as_tibble()
