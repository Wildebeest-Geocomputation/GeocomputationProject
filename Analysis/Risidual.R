library(spdep)

tif_files <- list.files(path = "./Data/Tif",
                        pattern = "\\.tif$",
                        full.names = TRUE,
                        ignore.case = TRUE)

presence <- rast(tif_files) %>%
  terra::classify(cbind(NA, 0)) %>%
  terra::mask(england_bng)

names(presence) <- file_path_sans_ext(basename(tif_files))

data_centers_sf <- read_csv("Data/Example/UK_Data_Centers.csv") %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326) %>%
  st_transform(27700)

# this is to thin the data to reduce spatial autocorrelation
raw_data <- read_csv("Data/Example/UK_Data_Centers.csv")
raw_data$species <- "DataCenter"
thinned_dataset <- thin(loc.data = raw_data,
                        lat.col = "lat", long.col = "lon",
                        spec.col = "species",
                        thin.par = 5, # km, if the data is too dense, remove one of the value
                        reps = 1,
                        locs.thinned.list.return = TRUE,
                        write.files = FALSE,
                        verbose = FALSE)
dim(thinned_dataset[[1]])[1]
dim(raw_data)[1] - dim(thinned_dataset[[1]])[1]

thinned_df <- thinned_dataset[[1]]
data_centers_sf <- st_as_sf(thinned_df, coords = c("Longitude", "Latitude"), crs = 4326) %>%
  st_transform(27700)

presence_vals <- terra::extract(presence, data_centers_sf, ID = FALSE)
valid_rows <- complete.cases(presence_vals)
presence_clean <- presence_vals[valid_rows, , drop = FALSE]

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

tmap_mode("plot")
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
          size = 0.2,
          title = "Model Residuals")


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

