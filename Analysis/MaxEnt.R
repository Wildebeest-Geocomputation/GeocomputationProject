library(sf)
library(maxnet)

tif_files <- list.files(path = "./Data/Tif",
                        pattern = "\\.tif$",
                        full.names = TRUE,
                        ignore.case = TRUE)

presence <- rast(tif_files)

library(tools)
names(presence) <- file_path_sans_ext(basename(tif_files))

data_centers_sf <- read_csv("Data/Example/UK_Data_Centers.csv") %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326) %>%
  st_transform(27700)

# This is to get the actual raster values for each data center location
# tidyr has a conflict with terra, so use :: to specify
presence_vals <- terra::extract(presence, data_centers_sf, ID = FALSE)

# This is to not getting error for maxent, there ar na now is because I crop the raster, so some data center doesn't have value
valid_rows <- complete.cases(presence_vals)
presence_clean <- presence_vals[valid_rows, , drop = FALSE]

bg_data <- spatSample(presence, size = 1000, method = "random", na.rm = TRUE, values = TRUE)
model_data <- as.data.frame(rbind(presence_clean, bg_data))

# The background point set to 0 because in entropy, presence points are 1, 0 represent random distributed points
set.seed(123)
pa <- c(rep(1, nrow(presence_clean)), rep(0, nrow(bg_data)))
# There is a bug in model, if the input only contain 1 variable, it will cause error bacause  [drop = FALSE]
me_model <- maxnet(p = pa, data = model_data)
suitability_map <- predict(presence, me_model, type = "logistic", na.rm = TRUE)
plot(suitability_map)

# This is response curve
plot(me_model, type = "logistic")

png("Data/SuitibilityMap/data_center_suitability.png", width = 2000, height = 2000, res = 300)
plot(suitability_map, main = "Data Center Suitability")
dev.off()

# AUC
library(ROCR)
pred <- predict(me_model, newdata = model_data, type = "logistic")
pred_obj <- prediction(pred, pa)
auc <- performance(pred_obj, measure = "auc")
auc@y.values[[1]]

pred_obj <- prediction(pred, pa)
roc_perf <- performance(pred_obj, measure = "tpr", x.measure = "fpr")

# Y (TPR, True Positive Rate / Sensitivity)
# X (FPR, False Positive Rate)
plot(roc_perf,
     main = "ROC Curve",
     ylab = "True Positive Rate (Sensitivity)",
     xlab = "False Positive Rate")
abline(a = 0, b = 1, lty = 2, col = "gray")
