
#Histograms
library(sf)
library(terra)
library(ggplot2)
library(dplyr)

base_path <- "C:/Users/Natal/1.GeocomputationProject/variables/"

solar_path <- paste0(base_path, "solar_irradiation_england.tif")
geology_path <- paste0(base_path, "geology_best.shp")
dc_path <- paste0(base_path, "UK_Data_Centers.csv")
flood_path <- paste0(base_path, "Flood_Risk_Areas.shp")
dc_sf <- read.csv(dc_path)
geology <- st_read(geology_path)
flood <- st_read(flood_path)
solar <- rast(solar_path)

dc_sf <- st_as_sf(dc, coords = c("lon", "lat"), crs = 4326)  # WGS84

# CRS 27700
target_crs <- "EPSG:27700"
dc_sf <- st_transform(dc_sf, target_crs)
geology <- st_transform(geology, target_crs)
flood <- st_transform(flood, target_crs)
solar <- project(solar, target_crs)

#Distance to geology
dist_geology_m <- st_distance(dc_sf, geology)
min_dist_geology_m <- apply(dist_geology_m, 1, min)
min_dist_geology_km <- as.numeric(min_dist_geology_m) / 1000

#Distance to flood_risk
dist_flood_m <- st_distance(dc_sf, flood)
min_dist_flood_m <- apply(dist_flood_m, 1, min)
min_dist_flood_km <- as.numeric(min_dist_flood_m) / 1000

#Distance to solar irradiation
dc_vect <- vect(dc_sf) #convert to spatvector to use terra
solar_values <- extract(solar, dc_vect, method = "bilinear")

solar_values_clean <- solar_values[, -1]


#Create dataframes
analysis_df <- data.frame(
  dc_id = 1:nrow(dc_sf),
  dist_geology_km = min_dist_geology_km,
  dist_flood_km = min_dist_flood_km,
  solar_value = solar_values_clean
)

write.csv(analysis_df,
          paste0(base_path, "distance_analysis_results.csv"),
          row.names = FALSE)

# Calcular estadísticas para anotaciones
median_geol <- median(min_dist_geology_km)
p95_geol <- quantile(min_dist_geology_km, 0.95)

summary(min_dist_geology_km)
summary(min_dist_flood_km)
summary(solar_values_clean)

# Calcular estadísticas para anotaciones
median_geol <- median(min_dist_geology_km)
p95_geol <- quantile(min_dist_geology_km, 0.95)

# Histograma completo
df_geology <- data.frame(distance_km = min_dist_geology_km)

p1_geology <- ggplot(df_geology, aes(x = distance_km)) +
  geom_histogram(binwidth = 5, fill = "lightblue", color = "darkblue", alpha = 0.7) +
  geom_vline(aes(xintercept = median_geol),
             color = "red", linetype = "dashed", linewidth = 1.2) +
  geom_vline(aes(xintercept = p95_geol),
             color = "orange", linetype = "dashed", linewidth = 1.2) +
  annotate("text", x = 5, y = Inf,
           label = paste("Median:", round(median_geol, 1), "km"),
           vjust = 1.5, hjust = 0, color = "red", size = 4, fontface = "bold") +
  annotate("text", x = 25, y = Inf,
           label = paste("95th %ile:", round(p95_geol, 1), "km"),
           vjust = 3, hjust = 0, color = "orange", size = 4, fontface = "bold") +
  labs(title = "Distribution of Data Centre Distances to Geology Features",
       subtitle = "England - Full Range",
       x = "Distance to Nearest Geology Feature (km)",
       y = "Count") +
  theme_minimal() +
  theme(plot.title = element_text(size = 14, face = "bold"))

print(p1_geology)
ggsave(paste0(base_path, "hist_geology_full.png"), p1_geology, width = 10, height = 6, dpi = 300)

# Histograma zoom 0-20km
df_geology_zoom <- df_geology[df_geology$distance_km <= 20, , drop = FALSE]

p2_geology <- ggplot(df_geology_zoom, aes(x = distance_km)) +
  geom_histogram(binwidth = 1, fill = "lightgreen", color = "darkgreen", alpha = 0.7) +
  geom_vline(xintercept = median_geol,
             color = "red", linetype = "dashed", linewidth = 1.2) +
  labs(title = "Data Centre Distances to Geology (Detail View)",
       subtitle = "0-20 km Range - England",
       x = "Distance to Nearest Geology Feature (km)",
       y = "Count") +
  scale_x_continuous(breaks = seq(0, 20, 2)) +
  theme_minimal() +
  theme(plot.title = element_text(size = 14, face = "bold"))

print(p2_geology)
ggsave(paste0(base_path, "hist_geology_zoom.png"), p2_geology, width = 10, height = 6, dpi = 300)

# Tabla de bins - Geología
cat("\n=== Data Centers by Distance Bins - GEOLOGÍA ===\n")
bins_geol <- c(0, 1, 2, 5, 10, 15, 20, 30, 50, 100, Inf)
bin_labels_geol <- c("0-1km", "1-2km", "2-5km", "5-10km", "10-15km",
                     "15-20km", "20-30km", "30-50km", "50-100km", ">100km")
bin_counts_geol <- cut(min_dist_geology_km, breaks = bins_geol, labels = bin_labels_geol)
table_geol <- table(bin_counts_geol)
print(table_geol)
print(prop.table(table_geol) * 100)  # Porcentajes

#________________________________________________________

median_flood <- median(min_dist_flood_km)
p95_flood <- quantile(min_dist_flood_km, 0.95)

df_flood <- data.frame(distance_km = min_dist_flood_km)

# Histograma completo
p1_flood <- ggplot(df_flood, aes(x = distance_km)) +
  geom_histogram(binwidth = 5, fill = "lightblue", color = "darkblue", alpha = 0.7) +
  geom_vline(aes(xintercept = median_flood),
             color = "red", linetype = "dashed", linewidth = 1.2) +
  geom_vline(aes(xintercept = p95_flood),
             color = "orange", linetype = "dashed", linewidth = 1.2) +
  annotate("text", x = 5, y = Inf,
           label = paste("Median:", round(median_flood, 1), "km"),
           vjust = 1.5, hjust = 0, color = "red", size = 4, fontface = "bold") +
  annotate("text", x = 25, y = Inf,
           label = paste("95th %ile:", round(p95_flood, 1), "km"),
           vjust = 3, hjust = 0, color = "orange", size = 4, fontface = "bold") +
  labs(title = "Distribution of Data Centre Distances to Flood Risk Areas",
       subtitle = "England - Full Range",
       x = "Distance to Nearest Flood Risk Area (km)",
       y = "Count") +
  theme_minimal() +
  theme(plot.title = element_text(size = 14, face = "bold"))

print(p1_flood)
ggsave(paste0(base_path, "hist_flood_full.png"), p1_flood, width = 10, height = 6, dpi = 300)

# Histograma zoom 0-20km
df_flood_zoom <- df_flood[df_flood$distance_km <= 20, , drop = FALSE]

p2_flood <- ggplot(df_flood_zoom, aes(x = distance_km)) +
  geom_histogram(binwidth = 1, fill = "lightgreen", color = "darkgreen", alpha = 0.7) +
  geom_vline(xintercept = median_flood,
             color = "red", linetype = "dashed", linewidth = 1.2) +
  annotate("text", x = median_flood + 1, y = Inf,
           label = paste("Median:", round(median_flood, 1), "km"),
           vjust = 1.5, hjust = 0, color = "red", size = 4, fontface = "bold") +
  labs(title = "Data Centre Distances to Flood Risk Areas (Detail View)",
       subtitle = "0-20 km Range - England",
       x = "Distance to Nearest Flood Risk Area (km)",
       y = "Count") +
  scale_x_continuous(breaks = seq(0, 20, 2)) +
  theme_minimal() +
  theme(plot.title = element_text(size = 14, face = "bold"))

print(p2_flood)
ggsave(paste0(base_path, "hist_flood_zoom.png"), p2_flood, width = 10, height = 6, dpi = 300)

# Tabla de bins - Flood
cat("\n=== Data Centers by Distance Bins - FLOOD RISK AREAS ===\n")
bins_flood <- c(0, 1, 2, 5, 10, 15, 20, 30, 50, 100, Inf)
bin_labels_flood <- c("0-1km", "1-2km", "2-5km", "5-10km", "10-15km",
                      "15-20km", "20-30km", "30-50km", "50-100km", ">100km")
bin_counts_flood <- cut(min_dist_flood_km, breaks = bins_flood, labels = bin_labels_flood)
table_flood <- table(bin_counts_flood)
print(table_flood)
print(prop.table(table_flood) * 100)

#___________________________________________________________

df_solar <- data.frame(solar_value = solar_values_clean)
df_solar <- df_solar[!is.na(df_solar$solar_value), , drop = FALSE]  # Remover NAs

median_solar <- median(df_solar$solar_value, na.rm = TRUE)
p95_solar <- quantile(df_solar$solar_value, 0.95, na.rm = TRUE)

# Calcular binwidth apropiado (aproximadamente 30 bins)
range_solar <- max(df_solar$solar_value) - min(df_solar$solar_value)
binwidth_solar <- range_solar / 30

p1_solar <- ggplot(df_solar, aes(x = solar_value)) +
  geom_histogram(binwidth = binwidth_solar, fill = "lightblue", color = "darkblue", alpha = 0.7) +
  geom_vline(aes(xintercept = median_solar),
             color = "red", linetype = "dashed", linewidth = 1.2) +
  geom_vline(aes(xintercept = p95_solar),
             color = "orange", linetype = "dashed", linewidth = 1.2) +
  annotate("text", x = min(df_solar$solar_value) + range_solar * 0.05, y = Inf,
           label = paste("Median:", round(median_solar, 1)),
           vjust = 1.5, hjust = 0, color = "red", size = 4, fontface = "bold") +
  annotate("text", x = median_solar + range_solar * 0.15, y = Inf,
           label = paste("95th %ile:", round(p95_solar, 1)),
           vjust = 3, hjust = 0, color = "orange", size = 4, fontface = "bold") +
  labs(title = "Distribution of Solar Irradiation at Data Centre Locations",
       subtitle = "England",
       x = "Solar Irradiation Value",
       y = "Count") +
  theme_minimal() +
  theme(plot.title = element_text(size = 14, face = "bold"))

print(p1_solar)
ggsave(paste0(base_path, "hist_solar.png"), p1_solar, width = 10, height = 6, dpi = 300)

# Tabla de bins - Solar (valores, no distancias)
cat("\n=== Data Centers by Solar Irradiation Bins ===\n")
# Ajusta estos rangos según los valores reales de tu raster
bins_solar <- quantile(df_solar$solar_value, probs = seq(0, 1, 0.2), na.rm = TRUE)
bin_counts_solar <- cut(df_solar$solar_value, breaks = bins_solar, include.lowest = TRUE)
table_solar <- table(bin_counts_solar)
print(table_solar)
print(prop.table(table_solar) * 100)

