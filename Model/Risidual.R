library(spdep)

presence_coords <- as.data.frame(st_coordinates(data_centers_sf[valid_rows, ]))
colnames(presence_coords) <- c("x", "y")
set.seed(123)
bg_sample <- spatSample(presence, size = 1000, method = "random", na.rm = TRUE, values = TRUE, xy = TRUE)

bg_coords <- bg_sample[, c("x", "y")]
bg_data <- bg_sample[, !names(bg_sample) %in% c("x", "y")]

model_data <- as.data.frame(rbind(presence_clean, bg_data))
pa <- c(rep(1, nrow(presence_clean)), rep(0, nrow(bg_data)))

pred_vals <- predict(me_model, model_data, type = "logistic")
residuals <- pa - pred_vals

# all_coords <- rbind(presence_coords, bg_coords)
all_coords <- rbind(presence_coords, bg_coords)

resid_df <- data.frame(
  x = all_coords$x,
  y = all_coords$y,
  resid = residuals
)

resid_sf <- st_as_sf(resid_df, coords = c("x", "y"), crs = 27700)

nb <- knn2nb(knearneigh(resid_sf, k = 10))
lw <- nb2listw(nb, style = "W") # w is row standardized, all weights sum to 1
moran_result <- moran.test(resid_sf$resid, lw)

print(moran_result)

tmap_mode("view")
tm_basemap("CartoDB.Positron") +
  tm_shape(england_bng) +
  tm_borders() +
  # tm_shape(resid_sf%>%filter(resid >0)) +
  tm_shape(resid_sf) +
  tm_compass()+
  tm_scale_bar()+
  tm_dots(col = "resid",
          midpoint = 0,
          style = "cont",
          palette = "RdBu",
          # size
          size = 0.5,
          title = "Model Residuals")

resid_sf%>%
  # filter(resid < 0)%>%
  ggplot() +
  geom_histogram(aes(x = resid), bins = 30, fill = "blue", alpha = 0.7)

england_polys <- st_as_sf(england_bng)

if (st_crs(england_polys) != st_crs(resid_sf)) {
  england_polys <- st_transform(england_polys, st_crs(resid_sf))
}
region_residuals <- england_polys %>%
  st_join(resid_sf%>%filter(resid<0)) %>%
  group_by(name) %>%
  summarise(mean_resid = mean(resid, na.rm = TRUE))%>%
  mutate(mean_resid = replace_na(mean_resid, 0))

tmap_mode("plot")

tm_basemap("CartoDB.Positron") +
  tm_shape(region_residuals) +
  tm_polygons("mean_resid", palette = "Reds", title = "Mean Residuals") +
  tm_compass(position = c("right", "top")) +
  tm_scale_bar(position = c("right", "bottom")) +
  tm_grid(labels.size = 0.7, n.x = 5, n.y = 5,
          lwd = 0.1,
          alpha = 0.5,
          labels.inside.frame = FALSE,
          projection = 27700)+
  tm_layout(
    legend.title.size = 0.9,
    main.title.size = 1,
    legend.outside = FALSE,
    legend.position = c("left", "top"),
    legend.bg.color = "white",
    legend.bg.alpha = 0.5,
    legend.frame = TRUE,
    legend.width = 9
  )

library(ncf)
coords <- st_coordinates(resid_sf)
x <- coords[, 1]
y <- coords[, 2]
z <- resid_sf$resid

# Correlogram
# increment for the uniformly distributed distance classes
# the number of permutations under the null to assess level of significance
correlog_result <- correlog(x, y, z, increment=5000, resamp=100)
# 550km started random
plot(correlog_result)
abline(h=0, lty=2, col="red")




