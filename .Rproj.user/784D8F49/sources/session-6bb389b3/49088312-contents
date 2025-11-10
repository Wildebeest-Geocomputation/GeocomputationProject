library(sf)
library(raster)
library(tidyverse)
library(adehabitatHR)
library(rnaturalearth)
library(rgeoboundaries)

# second-level administrative division
uk_l2 <- gb_adm2("United Kingdom")
uk_l2$area_km2 <- st_area(uk_l2) %>%
  units::set_units("km^2") %>%
  as.numeric()

file_path <- "Data/oprvrs_gb.gpkg"
st_layers(file_path)

hydro_node <- st_read(file_path, layer = "hydro_node")
water_link <- st_read(file_path, layer = "watercourse_link")
st_crs(hydro_node)
# plot(st_geometry(water_link), col = "blue", main = "Watercourse Links")
# plot(st_geometry(hydro_node), col = "red", add = TRUE)

# KDE
data_sp <- as_Spatial(hydro_node)
kde.output <- kernelUD(data_sp, h="href", grid = 1000)
h0 <- kde.output@h$h
kde.output <- kernelUD(data_sp, h=h0*0.1, grid = 1000)
plot(kde.output)
kde_trim <- raster(kde.output)
kde_trim[kde_trim <= 0] <- NA

tm_basemap(providers$Esri.WorldTopoMap)+
  tm_shape(kde_trim) +
  tm_raster(style = "quantile", n = 7, palette = "Reds", alpha = 0.5,
            legend.format = list(digits = 3, scientific = TRUE),
            title = "Hydro nodes density") +
  tm_shape(dc_sf) +
  tm_dots(size = 0.5, col = "#26b5ed", alpha = 0.7)+
  # tm_shape(uk_l2) +
  # tm_borders(col = "#731c1c", lwd = 0.5) +
  tm_layout(legend.outside = TRUE, frame = FALSE)

## Intersection
water_link <- st_transform(water_link, st_crs(uk_l2))
water_with_city <- st_join(
  water_link,
  uk_l2[, c("shapeName", "area_km2")],
  join = st_intersects,
  left = TRUE
)
city_water <- water_with_city%>%
  st_drop_geometry() %>%
  group_by(shapeName)%>%
  summarise(
    total_length = sum(length, na.rm = TRUE),
    avg_length = mean(length, na.rm = TRUE),
    total_area_km2 = first(area_km2),
    length_unit = total_length / total_area_km2,
    link_count = n(),
  )
uk_l2_with_water <- uk_l2 %>%
  left_join(city_water, by = "shapeName")
uk_l2_with_water <- uk_l2_with_water%>%
  mutate(
    length_unit = total_length / total_area_km2
  )

tm_basemap("CartoDB.Positron") +
  tm_shape(uk_l2_with_water) +
  tm_polygons(
    col = "length_unit",
    style = "equal",
    n = 5,
    palette = "Blues",
    border.col = "white",
    lwd = 0.3,
    title = "Unit Length (m)"
  ) +
  tm_shape(dc_sf) +
  tm_dots(size = 0.5, col = "#26b5ed", alpha = 0.7)+
  tm_shape(uk_l2_with_water %>% filter(length_unit > 1017)) +
  tm_text(
    text = "shapeName",
    size = 0.5,
    col = "black",
    shadow = TRUE,
    remove.overlap = TRUE
  )

tm_basemap("CartoDB.Positron") +
  tm_shape(uk_l2_with_water) +
  tm_polygons(
    col = "total_length",
    style = "equal",
    n = 5,
    palette = "Blues",
    border.col = "white",
    lwd = 0.3,
    title = "Unit Length (m)"
  ) +
  tm_shape(dc_sf) +
  tm_dots(size = 0.5, col = "#26b5ed", alpha = 0.7)+
  tm_shape(uk_l2_with_water %>% filter(length_unit > 1017)) +
  tm_text(
    text = "shapeName",
    size = 0.5,
    col = "black",
    shadow = TRUE,
    remove.overlap = TRUE
  )

# correlation plot
city_water%>%
  filter(
    link_count < quantile(link_count, 0.75) + 1.5 * IQR(link_count)
  )%>%
  ggplot()+
  geom_point(aes(x=link_count, y=total_length))+
  geom_text(
    aes(x=link_count, y=total_length, label=shapeName),
    check_overlap = TRUE,
    vjust = 0.5,
    hjust = -0.1,
    size = 2
  )+
  labs(
    title = "Watercourse Link Count vs Average Length",
    x = "Link Count",
    y = "Average Length (m)"
  )

# total_length and unit
city_water%>%
  filter(
    total_length < quantile(total_length, 0.75) + 1.5 * IQR(total_length)
  )%>%
  ggplot()+
  geom_point(aes(x=length_unit, y=total_length))+
  geom_text(
    aes(x=length_unit, y=total_length, label=shapeName),
    check_overlap = TRUE,
    vjust = 0.5,
    hjust = -0.1,
    size = 1
  )+
  labs(
    title = "Total Length vs Unit Length",
    x = "Unit Length (m/km^2)",
    y = "Total Length (m)"
)
# Water EDA
water_with_city%>%
  # remove outliers by 1.5*IQR
  filter(length < quantile(length, 0.75) + 1.5 * IQR(length))%>%
  ggplot()+
  geom_bar(aes(x=length)) +
  # add mean and median lines
  geom_vline(aes(xintercept=mean(length)), color="red", linetype="dashed", size=0.3) +
  geom_vline(aes(xintercept=median(length)), color="blue", linetype="dashed", size=0.3) +
  labs(title="Distribution of Watercourse Link Lengths",
       x="Length (meters)", y="Count") +
  theme_minimal()

water_with_city%>%
  filter(length < quantile(length, 0.75) + 1.5 * IQR(length))%>%
  ggplot()+
  geom_boxplot(aes(x=form, y=length)) +
  labs(x="Form", y="Length (meters)") +
  theme_minimal()

