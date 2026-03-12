library(sf)
library(terra)
library(readr)
library(maxnet)
library(tools)

# Read .tif from Rasters folder
tif_files <- c(
  "C:/Users/Natal/1.GeocomputationProject/MaxEntIndividual/Rasters/solar_suitability.tif",
  "C:/Users/Natal/1.GeocomputationProject/MaxEntIndividual/Rasters/non_floodrisk_suitability.tif",
  "C:/Users/Natal/1.GeocomputationProject/MaxEntIndividual/Rasters/geology_suitability.tif"
)

presence <- rast(tif_files)
names(presence) <- file_path_sans_ext(basename(tif_files))

# Read DC points
data_centers_sf <- read_csv("C:/Users/Natal/1.GeocomputationProject/variables/UK_Data_Centers.csv") %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326) %>%
  st_transform(27700)

# Extract raster values for each data center location
presence_vals <- terra::extract(presence, data_centers_sf, ID = FALSE)

# Remove rows with NA
valid_rows <- complete.cases(presence_vals)
presence_clean <- presence_vals[valid_rows, , drop = FALSE]

# Get background points
bg_data <- spatSample(presence, size = 500, method = "random", na.rm = TRUE, values = TRUE)

# Combine data
model_data <- as.data.frame(rbind(presence_clean, bg_data))

# Create a presence/absence vector
set.seed(123)
pa <- c(rep(1, nrow(presence_clean)), rep(0, nrow(bg_data)))

# MaxEnt
me_model <- maxnet(p = pa, data = model_data)

# Predict suitability map
suitability_map <- predict(presence, me_model, type = "logistic", na.rm = TRUE)
plot(suitability_map)

# Response curves
plot(me_model, type = "logistic")

# AUC calculation
library(ROCR)
pred <- predict(me_model, newdata = model_data, type = "logistic")
pred_obj <- prediction(pred, pa)
auc <- performance(pred_obj, measure = "auc")
cat("AUC:", auc@y.values[[1]], "\n")

# ROC Curve - stochastic variation
roc_perf <- performance(pred_obj, measure = "tpr", x.measure = "fpr")
plot(roc_perf,
     main = "ROC Curve",
     ylab = "True Positive Rate (Sensitivity)",
     xlab = "False Positive Rate")
abline(a = 0, b = 1, lty = 2, col = "gray")

# Confusion Matrix
library(caret)
library(tidyverse)

pred_factor <- factor(ifelse(pred > 0.3, 1, 0), levels = c(0, 1))
actual_factor <- factor(pa, levels = c(0, 1))

cm <- confusionMatrix(pred_factor, actual_factor)
print(cm)

# Confusion Matrix visualization
cm_table <- as.data.frame(cm$table)
cm_table %>%
  ggplot(aes(Reference, Prediction)) +
  geom_tile(aes(fill = Freq), color = "white") +
  scale_fill_gradient(low = "white", high = "steelblue") +
  geom_text(aes(label = Freq), vjust = 1) +
  labs(title = "Confusion Matrix", fill = "Count") +
  theme_minimal()


#Images format high quality
# Option 1: Ultra high-resolution PNG (300 DPI for print)
png(file.path(output_dir, "suitability_map.png"),
    width = 4000, height = 4000, res = 400, type = "cairo")
plot(suitability_map,
     main = "Data Center Suitability Map - MaxEnt Model",
     col = viridis::viridis(100),
     axes = TRUE,
     plg = list(title = "Probability", cex = 1.2),
     pax = list(cex.axis = 1.2),
     mar = c(3, 3, 3, 5))
dev.off()

# Individual curves - PNG ultra-high quality
png(file.path(output_dir, "response_curves_individual.png"),
    width = 4500, height = 1500, res = 300, type = "cairo")
par(mfrow = c(1, 3), mar = c(5, 5, 4, 2), cex.main = 1.5, cex.lab = 1.3, cex.axis = 1.2)
plot(me_model, type = "logistic")
dev.off()

# Calculate ROC
roc_perf <- performance(pred_obj, measure = "tpr", x.measure = "fpr")
auc <- performance(pred_obj, measure = "auc")
auc_value <- auc@y.values[[1]]

#High-quality PNG
png(file.path(output_dir, "roc_curve.png"),
    width = 2000, height = 2000, res = 300, type = "cairo")
par(mar = c(5, 5, 4, 2), cex.main = 1.5, cex.lab = 1.3, cex.axis = 1.2)
plot(roc_perf,
     main = sprintf("ROC Curve (AUC = %.4f)", auc_value),
     ylab = "True Positive Rate (Sensitivity)",
     xlab = "False Positive Rate",
     col = "steelblue",
     lwd = 3)
abline(a = 0, b = 1, lty = 2, col = "gray50", lwd = 2)
grid(col = "gray90")
legend("bottomright",
       legend = c(sprintf("Model (AUC=%.3f)", auc_value), "Random Chance"),
       col = c("steelblue", "gray50"),
       lwd = c(3, 2),
       lty = c(1, 2),
       cex = 1.2)
dev.off()
