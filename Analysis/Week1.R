library(sf)
library(tmap)
library(terra)
library(tidyverse)
library(tmaptools)

dat <- read.csv(file="Data/Temperature/Temp_China.csv")
roads <- st_read(dsn="Data/Stowe/Roads/roads.shp", layer="roads")
plot(roads)

elevation <- rast("Data/Stowe/Elevation/elevation1.tif")
plot(elevation)
plot(roads, add=T)

tm_shape(roads)+
  tm_lines(col="Shape_Leng", style="sd")+
  tm_scale_bar()+
  tm_compass()+
  tm_layout(title="Map of roads in Stowe")

schools <- st_read("Data/Stowe/Schools/schools.shp", "schools")
recsites <- st_read("Data/Stowe/Recsites/rec_sites.shp", "rec_sites")

recsites$ACREAGE <- as.numeric(recsites$ACREAGE)

ttm()
map <- leaflet::providers$OpenSnowMap.pistes
tm_basemap(map)+
  tm_shape(elevation)+
  tm_raster(alpha=0.3, title="Elevation (ft)")+
  tm_shape(roads)+
  tm_lines(col="Shape_Leng", style="jenks", palette="PuRd", lwd=2.0,
           title.col="Road length (metres)")+ # Use jenks classification
  tm_shape(recsites)+
  tm_dots(size="ACREAGE", title.size="Rec. Sites (Area, ac.)")+
  tm_shape(schools)+
  tm_dots(col="blue", alpha=0.7, size=1.5)+
  tm_scale_bar()+
  tm_compass()+
  tm_layout(legend.bg.color="white",
            title="Schools and Recreation Sites in Stowe",
            title.position = c("center", "bottom"),
            legend.outside = TRUE)

schoolsBuffer <- schools%>%st_buffer(dist=1000)%>%st_union()
roadsBuffer <- roads%>%st_buffer(dist=250)%>%st_union()
schoolSite <- st_difference(roadsBuffer, schoolsBuffer)
ttm() # Set map mode back to plot mode

roadsWGS <- st_transform(roads, 4326)
tm_shape(schools)+
  tm_dots(size=2)+
  tm_shape(schoolSite)+
  tm_polygons()+
  tm_shape(roads)+
  tm_lines()


# Calculate slope from elevation raster
slope <- terrain(elevation, v="slope")
tm_shape(slope)+ tm_raster()

# Create rasters of Euclidean distance from current school and recreation sites
schools_terra <- vect(schools)
crs(schools_terra) <- crs(elevation)
schoolsdist <- distance(elevation, schools_terra)
tm_shape(schoolsdist) + tm_raster()

terrainrecsites_terra <- vect(recsites)
crs(recsites_terra) <- crs(elevation)
recsitesdist <- distance(elevation, recsites_terra)
tm_shape(recsitesdist)+tm_raster()

# Reclassify each raster layer into a suitability score from 1:10
library(classInt)
slopeInt <- classIntervals(values(slope), n=10, style="equal")
slopeBrks <- cbind(slopeInt$brks[1:10], slopeInt$brks[2:11], 10:1)
slopeReclass <- classify(slope, slopeBrks)
tm_shape(slopeReclass) + tm_raster()

landuse <- rast("Data/Stowe/Landuse/landuse1.tif")
landuseBrks <- cbind(c(1,2,3,4,5,6,7), c(5,NaN,10,3,9,4,NaN))
landuseReclass <- classify(landuse, landuseBrks)
landuseResample <- resample(landuseReclass, elevation)
tm_shape(landuseResample)+ tm_raster()

recsitesInt <- classIntervals(values(recsitesdist), n=10, style="equal")
recsitesBrks <- cbind(recsitesInt$brks[1:10], recsitesInt$brks[2:11], 10:1)
recsitesReclass <- classify(recsitesdist, recsitesBrks)
tm_shape(recsitesReclass)+tm_raster()

schoolsInt <- classIntervals(values(schoolsdist), n=10, style="equal")
schoolsBrks <- cbind(schoolsInt$brks[1:10], schoolsInt$brks[2:11], 1:10)
schoolsReclass <- classify(schoolsdist, schoolsBrks)
tm_shape(schoolsReclass)+tm_raster()

tm_shape(slopeReclass) + tm_raster()
tm_shape(schoolsReclass)+tm_raster()
tm_shape(recsitesReclass)+tm_raster()
tm_shape(landuseResample)+ tm_raster()
# The weight is arbitrary
weighted_overlay <- ceiling(
  (slopeReclass*0.13)+(schoolsReclass*0.25)+
    (recsitesReclass*0.5)+(landuseResample*0.12))

tm_shape(weighted_overlay) + tm_raster()

hist(values(weighted_overlay), main="", xlab="Suitability score")

suitable_sites <- weighted_overlay >= 9
tm_shape(suitable_sites)+tm_raster()
