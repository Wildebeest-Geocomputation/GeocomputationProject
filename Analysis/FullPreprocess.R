#' This file combines RasterPreprocessNetwork and RasterPreprocessPoint to a function
#'
#' @param data Spatial data of points or lines, read RasterPreprocessNetwork & RasterPreprocessPoint for detail information
#' @param grid_size Grid size in meters, 1000 = 1km (in epsg 27700), decrease it if you want higher resolution
#' @param type 'network' for line data, 'point' for point data
#' @param save_name File name to save the output tif, if FALSE not saving
#' @param max_dist Maximum distance for fuzzy decrease function
#' @return A raster of suitability values
calculate_distance <- function(
    data, grid_size, type='network', save_name=FALSE, max_dist=10000
) {

  message('Generating grids from England boundaries')
  r_grid <- rast(ext(england_bng), resolution = grid_size, crs = "EPSG:27700")

  if (type=='network') {
    lines_latlon <- as.lines(data)
    lines <- project(lines_latlon, crs(r_grid))

    message('Calculating raster')
    res <- rasterize(lines, r_grid, field = 1, touches = TRUE)

  }
  else if (type=='point') {

    res <- rasterize(vect(data), r_grid, field = 1)

  }
  else {
    stop("Type must be either 'network' or 'point'")
  }

  dist_grid <- distance(res)
  plot(dist_grid)
  suitability <- app(dist_grid, fun = function(x) fuzzy_decrease(x, max_dist = max_dist))
  suitability <- mask(suitability, england_bng)
  plot(suitability)

  message('Save to tif')
  if(!is.na(save_name)) {
    writeRaster(suitability, filename=paste0(save_name, '.tif'), overwrite=TRUE)
  }

  return(suitability)
}
