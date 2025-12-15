library(geodata)

uk_level1 <- gadm(country="GBR", level=1, path=tempdir()) # main boundaries
uk_level2 <- gadm(country="GBR", level=2, path=tempdir()) # more detailed boundaries

others <- c("Scotland", "Wales", "Northern Ireland")
england <- uk_level1[(!uk_level1$NAME_1 %in% others & !is.na(uk_level1$GID_1)), ]
england_l2 <- uk_level2[!uk_level2$NAME_1 %in% others, ]
# england <- aggregate(england_counties)
# england_bng <- project(england, "EPSG:27700")

england_bng <- project(england, "EPSG:27700")
england_l2_bng <- project(england_l2, "EPSG:27700")
