install.packages("dismo")
install.packages("geodata")
install.packages("rnaturalearth")
install.packages("viridis")
library(terra)
library(sf)
library(dplyr)
library(ggplot2)
library(tidyr)
library(dismo) #MaxEnt algorithm library
library(geodata)
library(rnaturalearth)
library(tidyverse)
library(viridis)  # Para la paleta de colores


#calling datasets
dc_england <- st_read("C:/Temp_R/DC_England.gpkg")
geology_best <- st_read("C:/Temp_R/GeocomputationProject/geology_best_dc.gpkg")
flood_risk <- st_read("C:/Temp_R/Flood_Risk_Areas.shp/Flood_Risk_Areas.shp")
solar_irradiation <- rast("C:/Temp_R/solar_irradiation_england.tif")
england_boundaries <- st_read("C:/Temp_R/england_ctry_2022.shp")

# Lithology analysis BEFORE maxent
#Extract more suitable soils of Geology_England
geology_england <- st_read("C:/Temp_R/geology_england.gpkg")
geology_best <- geology_england %>%
  filter(ROCK_D %in% c("SAND AND GRAVEL", "SAND"))

#Download new layer
st_write(geology_best, "geology_best_dc.gpkg", delete_dsn = TRUE)
getwd()

#Geology_best suitability map (SAND AND GRAVEL + SAND)
geology_scored <- geology_england %>%
  mutate(
    suitability = case_when(
      ROCK_D == "SAND AND GRAVEL" ~ 3,
      ROCK_D == "SAND" ~ 2,
      ROCK_D == "DIAMICTON" ~ 1,
      TRUE ~ 0
    )
  )
plot(geology_scored["suitability"]) # Yellow and pink colours show higer suitability to place a DC

#England boundaries
# 1. INSTALAR PAQUETES NECESARIOS (solo primera vez)
install.packages("rnaturalearth")
install.packages("rnaturalearthdata")
install.packages("remotes") #rnaturalearthhires no está en CRAN, hay que descargarlo desde GIT con remotes

remotes::install_github("ropensci/rnaturalearthhires") #Para instalar desde GIT

# PREPARACIÓN DE DATOS PARA MAXENT - Adaptado al flujo de referencia
# Resolución: 1km, CRS: BNG (EPSG:27700)
library(sf)
library(terra)

england_boundaries <- st_read("C:/Temp_R/england_ctry_2022.shp")

class(england_boundaries)
st_bbox(england_boundaries)
st_crs(england_boundaries)

# Convert sf → SpatVector
england_vect <- vect(england_boundaries)

# Resolución 1 km
grid_res <- 1000

# Crear raster base
r_grid <- rast(
  ext(england_vect),
  resolution = grid_res,
  crs = crs(england_vect)
)

# Asignar ID a cada celda
values(r_grid) <- 1:ncell(r_grid)

# Mask para quedarnos solo con England
r_grid_masked <- mask(r_grid, england_vect)

# Comprobación visual
plot(r_grid_masked, main = "1 km grid – England (BNG)", axes = FALSE)

#GEOLOGY
geology_best <- st_read("C:/Temp_R/GeocomputationProject/geology_best_dc.gpkg")

# Transformar a BNG
geology_bng <- st_transform(geology_best, crs = 27700)

# Intersectar con England
cat("  Intersectando con England...\n")
geology_england <- st_intersection(geology_bng, england_bng)
cat("  Geometrías de geology:", nrow(geology_england), "\n")

# Rasterizar
cat("  Rasterizando...\n")
r_geology <- rasterize(vect(geology_england), r_grid_masked, field = 1)

# Calcular distancia
cat("  Calculando distancias... (esto puede tardar)\n")
dist_geology <- distance(r_geology)

# Normalizar (invertir: cerca = 1, lejos = 0)
geology_max <- global(dist_geology, "max", na.rm = TRUE)[[1]]
suitability_geology <- 1 - (dist_geology / geology_max)
suitability_geology <- mask(suitability_geology, england_vect)

cat("✓ Geology suitability calculado\n")
cat("  Rango:",
    round(global(suitability_geology, "min", na.rm = TRUE)[[1]], 3), "-",
    round(global(suitability_geology, "max", na.rm = TRUE)[[1]], 3), "\n\n")

plot(suitability_geology, main = "Geology Suitability", col = terrain.colors(100))

# PLOT PERSONALIZADO GEOLOGY SUITABILITY
cat("1. Geology Suitability\n")
plot_suitability(suitability_geology,
                 "Geology Suitability",
                 england_bng)

#______________________________________________________________________________________#

#PARTE 2 GEOLOGY
#Convertir Rock_ID attribute to numbers para que maxent lo entienda
geology_boundaries$rock_id <- as.numeric(
  as.factor(geology_boundaries$ROCK_D)
)
levels(as.factor(geology_boundaries$ROCK_D))

#Rasterizar Geology sobre la misma grid de 1 km
geology_r <- rasterize(
  vect(geology_boundaries),
  r_grid_masked,
  field = "rock_id"
)

#Convertir a factor clave para MaxEnt
geology_r <- as.factor(geology_r)

#Visualización (raster de geology a 1 km de resolución,con BNG 27700, alineado a r_grid_masked, listo para maxent)
plot(
  geology_r,
  main = "Geology suitability (1 km, BNG)",
  axes = FALSE
)

#2. FLOOD RISK

#Asegurar CRS BNG
flood_risk <- st_transform(flood_risk, 27700)
st_crs(flood_risk)

#Convertir a SpatVector
flood_vect <- vect(flood_risk)

# Crear una columna binaria para rasterización
# 1 = riesgo de inundación
flood_risk <- flood_risk %>%
  mutate(flood_bin = 1)

#Creación de raster
grid_res <- 1000 # 1 km
r_grid <- rast(ext(flood_risk), resolution = grid_res, crs = st_crs(flood_risk)$proj4string)
values(r_grid) <- 1:ncell(r_grid)

# Rasterizar flood_risk sobre el grid
r_flood <- rasterize(vect(flood_risk), r_grid, field = "flood_bin")

#Calcular distancias a zonas inundables
dist_flood <- distance(r_flood)

#Distancia a celdas de riesgo verificación
summary(values(dist_flood))  # ahora debería dar valores > 0

#Transformar a valores donde 1 = lejos (bueno), 0 = cerca (malo)
max_dist <- 5000  # 5 km
suitability_flood <- app(dist_flood, fun = function(x) {
  y <- x / max_dist
  y[y > 1] <- 1
  y[y < 0] <- 0
  return(y)
})

# --- Plot del raster de aptitud ---
plot(suitability_flood, main="Data Centers suitability (1 = Far from floods)")



#FLOODS OPTION 2
flood_risk <- st_read("C:/Temp_R/Flood_Risk_Areas.shp/Flood_Risk_Areas.shp")
flood_bng <- st_transform(flood_risk, crs = 27700)

# Intersectar con England
cat("  Intersectando con England...\n")
flood_england <- st_intersection(flood_bng, england_bng)

# Rasterizar
cat("  Rasterizando...\n")
r_flood <- rasterize(vect(flood_england), r_grid_masked, field = 1)

# Calcular distancia
cat("  Calculando distancias... (esto puede tardar)\n")
dist_flood <- distance(r_flood)

# Normalizar (NO invertir: lejos = 1 = bueno, cerca = 0 = malo)
flood_max <- global(dist_flood, "max", na.rm = TRUE)[[1]]
suitability_flood <- dist_flood / flood_max
suitability_flood <- mask(suitability_flood, england_vect)

cat("✓ Flood suitability calculado\n")
cat("  Rango:",
    round(global(suitability_flood, "min", na.rm = TRUE)[[1]], 3), "-",
    round(global(suitability_flood, "max", na.rm = TRUE)[[1]], 3), "\n\n")

plot(suitability_flood, main = "Flood Risk Suitability", col = heat.colors(100))

#PLOT PERSONALIZADO FLOOD RISK
# ============================================================================
# FUNCIÓN PARA PLOTEAR CON ESTILO PERSONALIZADO
# ============================================================================

plot_suitability <- function(raster_data, title, england_boundary) {

  # Configurar márgenes
  par(mar = c(2, 2, 3, 5))

  # Plot del raster con paleta viridis
  plot(raster_data,
       main = title,
       col = viridis(100),  # Paleta viridis (morado a amarillo)
       axes = FALSE,
       box = FALSE,
       legend = TRUE,
       range = c(0, 1),  # Rango fijo de 0 a 1
       plg = list(  # Configuración de la leyenda
         title = "Suitability",
         title.cex = 0.9,
         cex = 0.8
       ),
       pax = list(  # Configuración de los ejes
         cex.axis = 0.8
       )
  )

  # Agregar contorno de England
  plot(st_geometry(england_boundary),
       add = TRUE,
       border = "cyan",  # Color cyan como en la imagen
       lwd = 1.5)
}

cat("2. Flood Risk Suitability\n")
plot_suitability(suitability_flood,
                 "Flood Risk Suitability",
                 england_bng)

# 3. SOLAR IRRADIATION
solar_irradiation <- rast("C:/Temp_R/solar_irradiation_england.tif")

#Reproyectar y resamplear

solar_bng <- project(solar_irradiation, "EPSG:27700")
solar_1km <- resample(solar_bng, r_grid_masked, method = "bilinear")
solar_england <- mask(solar_1km, england_bng)


# ============================================================================
# PASO 1: Cargar england_bng
# ============================================================================

cat("  CRS original:", st_crs(england_boundaries)$input, "\n")
cat("  Filas:", nrow(england_boundaries), "\n")

#Transformar a BNG
england_bng <- st_transform(england_boundaries, crs = 27700)

cat("✓ England transformado a BNG\n")
cat("  CRS nuevo:", st_crs(england_bng)$input, "\n")
cat("  Bbox:\n")
print(st_bbox(england_bng))

if(nrow(england_bng) > 1) {
  cat("  Uniendo", nrow(england_bng), "geometrías...\n")
  england_bng <- england_bng %>%
    st_union() %>%
    st_as_sf()
}

#Reparar geometrías si es necesario
if(!all(st_is_valid(england_bng))) {
  cat("  Reparando geometría...\n")
  england_bng <- st_make_valid(england_bng)
}

st_write(england_bng, "C:/Temp_R/england_bng.gpkg", delete_dsn = TRUE, quiet = TRUE)
cat("✓ Guardado en: C:/Temp_R/england_bng.gpkg\n\n")

# ============================================================================
# PASO 2: Convertir england_bng a SpatVector (terra)
# ============================================================================

# IMPORTANTE: Convertir a SpatVector ANTES de crear el grid
england_vect <- vect(england_bng)
#Verificar CRS
cat("  CRS de england_vect:", crs(england_vect), "\n")

#Definir resolución
grid_res <- 1000  # 1km

# Crear grid base
r_grid <- rast(ext(england_vect), resolution = grid_res, crs = crs(england_vect))
values(r_grid) <- 1:ncell(r_grid)

cat("  Grid base:", ncol(r_grid), "x", nrow(r_grid), "=",
    ncol(r_grid) * nrow(r_grid), "celdas\n")

# Enmascarar solo a England
r_grid_masked <- mask(r_grid, england_vect)

# Visualizar
plot(r_grid_masked, main = "England Grid 1km", axes = FALSE, legend = FALSE,
     col = "lightblue")

#Procesar Solar Irradiation
cat("  CRS original:", crs(solar_irradiation), "\n")
cat("  Dimensiones originales:", nrow(solar_irradiation), "x",
    ncol(solar_irradiation), "\n")

#Proyectar a BNG como CRS
cat("  Reproyectando a BNG...\n")
solar_bng <- project(solar_irradiation, "EPSG:27700")

# Resamplear a 1km (mismo grid que r_grid_masked)
cat("  Resampleando a 1km...\n")
solar_1km <- resample(solar_bng, r_grid_masked, method = "bilinear")

# Enmascarar a England
solar_england <- mask(solar_1km, england_vect)

# Normalizar valores entre 0 y 1
cat("  Normalizando valores...\n")
solar_min <- global(solar_england, "min", na.rm = TRUE)[[1]]
solar_max <- global(solar_england, "max", na.rm = TRUE)[[1]]

cat("  Valores originales - Min:", round(solar_min, 2),
    "Max:", round(solar_max, 2), "\n")

suitability_solar <- (solar_england - solar_min) / (solar_max - solar_min)

cat("✓ Solar suitability calculado\n")
cat("  Rango: 0 - 1 (normalizado)\n\n")

plot(suitability_solar, main = "Solar Suitability", col = heat.colors(100))

cat("PASO 6: Creando stack final para MaxEnt...\n")

#PLOT PERSONALIZADO SOLAR SUITABILITY
cat("3. Solar Suitability\n")
plot_suitability(suitability_solar,
                 "Solar Suitability",
                 england_bng)


#SAFE RASTERS AS .TIF
# Crear directorio si no existe
dir.create("C:/Temp_R/Data/Tif", showWarnings = FALSE, recursive = TRUE)

#guardar capas individuales

cat("Guardando rasters...\n\n")

# 1. GEOLOGY SUITABILITY
cat("1. Guardando geology_suitability_1km.tif...\n")
writeRaster(suitability_geology,
            "C:/Temp_R/Data/Tif/geology_suitability_1km.tif",
            overwrite = TRUE,
            datatype = "FLT4S")  # Formato float de 32 bits
cat("   ✓ Guardado exitosamente\n")
cat("   Rango de valores:",
    round(global(suitability_geology, "min", na.rm = TRUE)[[1]], 3), "-",
    round(global(suitability_geology, "max", na.rm = TRUE)[[1]], 3), "\n\n")


# 2. FLOOD RISK SUITABILITY
cat("2. Guardando flood_suitability_1km.tif...\n")
writeRaster(suitability_flood,
            "C:/Temp_R/Data/Tif/flood_suitability_1km.tif",
            overwrite = TRUE,
            datatype = "FLT4S")
cat("   ✓ Guardado exitosamente\n")
cat("   Rango de valores:",
    round(global(suitability_flood, "min", na.rm = TRUE)[[1]], 3), "-",
    round(global(suitability_flood, "max", na.rm = TRUE)[[1]], 3), "\n\n")

# 3. SOLAR SUITABILITY
cat("3. Guardando solar_suitability_1km.tif...\n")
writeRaster(suitability_solar,
            "C:/Temp_R/Data/Tif/solar_suitability_1km.tif",
            overwrite = TRUE,
            datatype = "FLT4S")
cat("   ✓ Guardado exitosamente\n")
cat("   Rango de valores:",
    round(global(suitability_solar, "min", na.rm = TRUE)[[1]], 3), "-",
    round(global(suitability_solar, "max", na.rm = TRUE)[[1]], 3), "\n\n")


# ============================================================================
# VERIFICAR ARCHIVOS GUARDADOS
# ============================================================================

cat("════════════════════════════════════════════════════════\n")
cat("  VERIFICACIÓN DE ARCHIVOS\n")
cat("════════════════════════════════════════════════════════\n\n")

# Listar archivos .tif en el directorio
tif_files <- list.files("C:/Temp_R/Data/Tif", pattern = "\\.tif$", full.names = TRUE)

cat("Archivos .tif guardados:\n\n")
for(i in seq_along(tif_files)) {
  file_info <- file.info(tif_files[i])
  file_size <- round(file_info$size / 1024 / 1024, 2)  # Tamaño en MB
  cat(paste0("  ", i, ". ", basename(tif_files[i]), "\n"))
  cat(paste0("     Ubicación: ", tif_files[i], "\n"))
  cat(paste0("     Tamaño: ", file_size, " MB\n\n"))
}

# ============================================================================
# VERIFICAR QUE LOS ARCHIVOS SE PUEDEN LEER
# ============================================================================

cat("Verificando que los archivos se pueden leer correctamente...\n\n")

# Leer cada archivo para verificar
geology_check <- rast("C:/Temp_R/Data/Tif/geology_suitability_1km.tif")
cat("✓ geology_suitability_1km.tif - Dimensiones:",
    nrow(geology_check), "x", ncol(geology_check), "\n")

flood_check <- rast("C:/Temp_R/Data/Tif/flood_suitability_1km.tif")
cat("✓ flood_suitability_1km.tif - Dimensiones:",
    nrow(flood_check), "x", ncol(flood_check), "\n")

solar_check <- rast("C:/Temp_R/Data/Tif/solar_suitability_1km.tif")
cat("✓ solar_suitability_1km.tif - Dimensiones:",
    nrow(solar_check), "x", ncol(solar_check), "\n\n")

# RESUMEN FINAL
# ============================================================================

cat("════════════════════════════════════════════════════════\n")
cat("         ✓✓✓ ARCHIVOS GUARDADOS EXITOSAMENTE ✓✓✓\n")
cat("════════════════════════════════════════════════════════\n\n")

cat("UBICACIÓN: C:/Temp_R/Data/Tif/\n\n")

cat("ARCHIVOS CREADOS:\n")
cat("  1. geology_suitability_1km.tif\n")
cat("  2. flood_suitability_1km.tif\n")
cat("  3. solar_suitability_1km.tif\n\n")

cat("CARACTERÍSTICAS DE CADA ARCHIVO:\n")
cat("  • Formato: GeoTIFF\n")
cat("  • Tipo de datos: Float 32 bits\n")
cat("  • Resolución: 1 km (1000 metros)\n")
cat("  • CRS: EPSG:27700 (British National Grid)\n")
cat(paste("  • Dimensiones:", nrow(geology_check), "filas x",
          ncol(geology_check), "columnas\n"))
cat("  • Valores: 0.0 (baja suitability) a 1.0 (alta suitability)\n\n")

cat("INTERPRETACIÓN:\n")
cat("  • geology_suitability_1km.tif:\n")
cat("    1.0 = sobre suelo idóneo | 0.0 = lejos de suelo idóneo\n\n")
cat("  • flood_suitability_1km.tif:\n")
cat("    1.0 = lejos de zonas de riesgo | 0.0 = cerca de zonas de riesgo\n\n")
cat("  • solar_suitability_1km.tif:\n")
cat("    1.0 = alta irradiación solar | 0.0 = baja irradiación solar\n\n")

cat("════════════════════════════════════════════════════════\n")
cat("  LISTO PARA USAR EN MAXENT\n")
cat("════════════════════════════════════════════════════════\n\n")

cat("PARA MAXENT:\n")
cat("  1. Coloca estos 3 archivos .tif en una carpeta\n")
cat("  2. En MaxEnt, selecciona esta carpeta como 'Environmental layers'\n")
cat("  3. MaxEnt detectará automáticamente los 3 rasters\n\n")

shell.exec("C:/Temp_R/Data/Tif")



