#' This file combines RasterPreprocessNetwork and RasterPreprocessPoint to a function
#'
#' @param data Spatial data of points or lines, read RasterPreprocessNetwork & RasterPreprocessPoint for detail information
#' @param grid_size Grid size in meters, 1000 = 1km (in epsg 27700), decrease it if you want higher resolution
#' @param type 'network' for line data, 'point' for point data, 'area' for area data with value
#' @param save_name File name to save the output tif, if FALSE not saving
#' @param max_dist Maximum distance for fuzzy decrease function
#' @param suitability_type Type of suitability function
#' @return A raster of suitability values
calculate_distance <- function(
    data, grid_size, type='network', save_name=FALSE, max_dist=10000, suitability_type='decrease', area_value=FALSE
) {

  data <- data%>%
    st_transform(crs = "EPSG:27700")%>%
    st_filter(st_as_sf(england_bng))

  message('Generating grids from England boundaries')
  r_grid <- rast(ext(england_bng), resolution = grid_size, crs = "EPSG:27700")

  if (type=='network') {
    lines_latlon <- as.lines(data)
    lines <- project(lines_latlon, crs(r_grid))

    message('Calculating raster')
    res <- rasterize(lines, r_grid, field = 1, touches = TRUE)

  }
  else if (type=='point') {
    # This is also suitable for polygon like brownfield sites
    res <- rasterize(vect(data), r_grid, field = 1)
  }
  else if (type == 'area') {
    # Use this if your area has a value
    res <- rasterize(vect(data), r_grid, field = area_value)
  }
  else {
    stop("Type must be either 'network' or 'point' or 'area'")
  }

  if (type != 'area') {

    dist_grid <- distance(res)
    plot(dist_grid)
    if (suitability_type == 'decrease') {
      suitability <- app(dist_grid, fun = function(x) fuzzy_decrease(x, max_dist = max_dist))
    } else if (suitability_type == 'increase') {
      suitability <- app(dist_grid, fun = function(x) fuzzy_increase(x, min_dist = max_dist))
    } else {
      stop("suitability_type must be either 'decrease' or 'increase'")
    }
    suitability <- mask(suitability, england_bng)
  }
  else {
    min_val <- global(res, "min", na.rm=TRUE)[1,1]
    max_val <- global(res, "max", na.rm=TRUE)[1,1]

    r_scaled <- (res - min_val) / (max_val - min_val)
    plot(r_scaled)

    suitability <- r_scaled
  }

  plot(suitability)
  if(!is.na(save_name)) {
    message('Save to tif')
    writeRaster(suitability, filename=paste0(save_name, '.tif'), overwrite=TRUE)
  }

  return(suitability)
}
