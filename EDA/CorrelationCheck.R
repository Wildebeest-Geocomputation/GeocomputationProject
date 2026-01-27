library(terra)
library(corrplot)

# Load all tif files
tif_files <- list.files(path = "./Data/Tif", pattern = "\\.tif$", full.names = TRUE)
selected_files <- tif_files#[c(1,2,3,4,5,7,8,9,10,11,12)]
env_stack <- rast(selected_files)

# Rename the variables by their file names
clean_names <- tools::file_path_sans_ext(basename(tif_files))
<<<<<<< HEAD:Analysis/correlation_check.R
names(env_stack) <- c("Brownfields", "Drought Severity Index", "Time to Large Employers", "Geology", "Flood Risk Areas", "Major Roads", "Solar Irradiation", "Underground Cables", "Overhead Lines", "Substations", "Annual Median Air Temperature")
=======
# names(env_stack) <- c("Brownfield", "Drought Severity Index", "Journey Time to Large Employers", "Geology", "Flood Risk Areas", "Major Roads", "Solar Radiation", "Underground Cables", "Overhead Lines", "Substations", "Annual Average Air Temperature")
>>>>>>> 7152954a24f2c4794a97028c7f41e28fd4f544af:EDA/CorrelationCheck.R

# Extract a sample
set.seed(123) # consistent results
env_sample <- spatSample(env_stack, size = 50000, method = "random", na.rm = TRUE, values = TRUE)

# Calculate the correlation
cor_matrix <- cor(env_sample, method = "spearman", use = "complete.obs")

# Plot the correlation matrix
corrplot(cor_matrix,
         method = "color",       # Show color tiles
         type = "upper",         # Only show the top half (cleaner)
         addCoef.col = "black",  # Add the number value on top
         tl.col = "black",       # Text label color
         tl.cex = 0.8,           # Text size
         number.cex = 0.7,       # Number size
         diag = FALSE,           # Hide the diagonal (1.0)
         col = COL2('RdBu', 10), # Red (Neg) to Blue (Pos) color scale
         #title = "Variable Correlation Matrix",
         mar = c(0,0,1,0))       # Fix margins for title
