# Data centers within 10m of roads: 0
# Data centers within 50m of roads: 12
# Data centers within 100m of roads: 41
# Percentage within 100m: 8.6 %

library(tmap)
library(ggplot2)
library(sf)
source('~/GeocomputationProject/utils/boundaries.R')  # Load england_bng


roads=st_read("~/STA_MSc/Term_1/SAGc/Group assignment/Data/strategic_rds.shp")
emp=st_read("~/STA_MSc/Term_1/SAGc/Group assignment/Data/emp_5000.gpkg")
dc_england <- st_read("./Data/dc_england.shp")

# plot the distribution of how long it takes to get to a large employer
mean(emp$X5000EmpAvgt, na.rm = T) # the median average time - 24.09 minutes
ggplot(data=emp, aes(X5000EmpAvgt)) + geom_histogram() + labs(x="Average time to large Employers (In minutes)")

# Second plot
# Calculate distances (now only for England DCs)
distances_to_roads <- st_distance(dc_england, roads)
min_distances <- apply(distances_to_roads, 1, min)
min_distances_km <- min_distances / 1000
median_dist <- median(min_distances_km)
percentile_95 <- quantile(min_distances_km, 0.95)

# Histogram
df_dist <- data.frame(distance_km = min_distances_km)

# Full range with wider bins (for overview)
ggplot(df_dist, aes(x = distance_km)) +
  geom_histogram(binwidth = 5, fill = "lightgreen", color = "darkgreen", alpha = 0.7) +
  geom_vline(aes(xintercept = median_dist),
             color = "red", linetype = "dashed", linewidth = 1.2) +
  geom_vline(aes(xintercept = percentile_95),
             color = "orange", linetype = "dashed", linewidth = 1.2) +
  annotate("text", x = 5, y = Inf,
           label = paste("Median:", round(median_dist, 1), "km"),
           vjust = 1.5, hjust = 0, color = "red", size = 4, fontface = "bold") +
  annotate("text", x = 25, y = Inf,
           label = paste("95th %ile:", round(percentile_95, 1), "km"),
           vjust = 3, hjust = 0, color = "orange", size = 4, fontface = "bold") +
  labs(title = "Distribution of Data Centre Distances to Major Roads",
       subtitle = "England - Full Range",
       x = "Distance to Nearest Road (km)",
       y = "Count") +
  theme_minimal() +
  theme(plot.title = element_text(size = 14, face = "bold"))

# Zoomed 0-20km histogram - create a proper filtered dataframe first
df_dist_zoom <- df_dist[df_dist$distance_km <= 26, , drop = FALSE]

ggplot(df_dist_zoom, aes(x = distance_km)) +
  geom_histogram(binwidth = 1, fill = "lightblue", color = "darkblue", alpha = 0.7) +
  geom_vline(xintercept = median_dist,
             color = "red", linetype = "dashed", linewidth = 1.2) +
  geom_vline(aes(xintercept = percentile_95),
             color = "orange", linetype = "dashed", linewidth = 1.2) +
  annotate("text", x = median_dist + 1, y = Inf,
           label = paste("Median:", round(median_dist, 1), "km"),
           vjust = 1.5, hjust = 0, color = "red", size = 4, fontface = "bold") +
  annotate("text", x = 25, y = Inf,
           label = paste("95th %ile:", round(percentile_95, 1), "km"),
           vjust = 3, hjust = 1, color = "orange", size = 4, fontface = "bold") +
  labs(title = "Data Centre Distances to Major Roads",
       #subtitle = "0-20 km Range - England",
       x = "Distance to Nearest Road (km)",
       y = "Count") +
  scale_x_continuous(breaks = seq(0, 26, 2)) +
  theme_minimal() +
  theme(plot.title = element_text(size = 14, face = "bold"))

# Count by distance bins
cat("\n=== Data Centers by Distance Bins ===\n")
bins <- c(0, 1, 2, 5, 10, 15, 20, 30, 50, 100, Inf)
bin_labels <- c("0-1km", "1-2km", "2-5km", "5-10km", "10-15km",
                "15-20km", "20-30km", "30-50km", "50-100km", ">100km")
bin_counts <- cut(min_distances_km, breaks = bins, labels = bin_labels)
table_counts <- table(bin_counts)
print(table_counts)
