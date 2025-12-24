tryCatch({
  # sometimes this doesn't work with no reason, try this again and it'll be fine
  library(rnaturalearth)
  message("Trying rnaturalearth ")

  uk_sf <- ne_states(country = "united kingdom", returnclass = "sf")
  england_sf <- uk_sf[uk_sf$geonunit == "England", ]
  england <- vect(england_sf)
  england_bng <- project(england, "EPSG:27700")
  plot(england_bng, main="England Boundary (via rnaturalearth)")

}, error=function(e){

  library(geodata)
  message("Trying geodata")
  uk_level1 <- gadm(country="GBR", level=1, path=tempdir()) # main boundaries
  uk_level2 <- gadm(country="GBR", level=2, path=tempdir()) # more detailed boundaries

  others <- c("Scotland", "Wales", "Northern Ireland")
  england <- uk_level1[(!uk_level1$NAME_1 %in% others & !is.na(uk_level1$GID_1)), ]
  england_l2 <- uk_level2[!uk_level2$NAME_1 %in% others, ]

  england_bng <- project(england, "EPSG:27700")
  england_l2_bng <- project(england_l2, "EPSG:27700")
})

