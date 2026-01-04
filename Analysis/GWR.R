# Need to run MaxEnt.R first to get the variables
if (!require("GWmodel")) install.packages("GWmodel")
library(GWmodel)
library(sf)
library(sp)
library(terra)
library(tidyverse)

set.seed(123)

# GWR is based on point data, not grid, so we need to extract the point data
coords_pres <- st_coordinates(data_centers_sf[valid_rows, ])
# GWR need x,y to calculate distance
bg_sample <- spatSample(presence, size = 1000, method = "random",
                        na.rm = TRUE, xy = TRUE, values = TRUE)

abs_bg <- bg_sample[, c("x", "y")]
data_bg <- bg_sample[, !names(bg_sample) %in% c("x", "y")]

gwr_coords <- rbind(coords_pres, as.matrix(abs_bg))
gwr_predictors <- rbind(presence_clean, data_bg)
gwr_pa <- c(rep(1, nrow(presence_clean)), rep(0, nrow(data_bg)))

gwr_df <- data.frame(pa = gwr_pa, gwr_predictors)
gwr_spdf <- SpatialPointsDataFrame(coords = gwr_coords, data = gwr_df)
# crs(gwr_spdf) <- crs(presence)

var_names <- names(presence)
gwr_eqn <- as.formula(paste("pa ~", paste(var_names, collapse = " + ")))
print(gwr_eqn)

dMat <- gw.dist(dp.locat = coordinates(gwr_spdf))
# calcuate best bandwidth
# adaptive=TRUE means using fixed number of neighbors (suitable for uneven point distribution)
# This take time to iterate
# bw <- bw.ggwr(gwr_eqn,
#               data = gwr_spdf,
#               family = "binomial",
#               approach = "AIC",
#               adaptive = TRUE,
#               dMat = dMat)
# bw

# I'll have two r-square values, one for logistic and one for GWR
# Logistic means that it assumes all coefficients are constant across space
# GWR means that coefficients vary across space
# Null Deviance: assume all coefficients are zero, guessing a number
# Global Residual Deviance: include all coefficients
# GW Deviance: include coefficients that vary across space
ggwr_model <- ggwr.basic(gwr_eqn,
                         data = gwr_spdf,
                         bw = 614,
                         # bw = bw,
                         family = "binomial",
                         adaptive = TRUE)

print(ggwr_model)
results_sf <- st_as_sf(ggwr_model$SDF)

# Coefficient distribution summary
# red for positive influence, blue for negative
first_var_name <- 'tas_annual_8100_median'

plot(results_sf['tas_annual_8100_median'],
     main = paste("Local Coefficients for:", first_var_name),
     pch = 20, cex = 0.5)
