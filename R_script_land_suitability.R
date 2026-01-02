# Geology, solar irradiation and flood risk datasets

library(terra)
library(sf)
library(dplyr)
library(ggplot2)
library(tidyr)

#calling datasets
dc_england <- st_read("C:/Temp_R/DC_England.gpkg")
geology_england <- st_read("C:/Temp_R/geology_england.gpkg")
flood_risk <- st_read("C:/Temp_R/Flood_risk_FZ3.gpkg")
flood_risk_1 <- st_read("C:/Temp_R/Flood_Risk_Areas.shp/Flood_Risk_Areas.shp")
solar_irradiation <- rast("C:/Temp_R/solar_irradiation_england.tif")
england_boundaries <- st_read("C:/Temp_R/england_ctry_2022.shp")
plot(geology_england)


#Geology map______________________________________________________________
library(sf)
library(tmap)

# Cargar capa
geology_england <- st_read("C:/Temp_R/geology_england.gpkg", stringsAsFactors = FALSE)

tm_shape(geology_england) +
  tm_polygons(
    "ROCK_D",
    title = "Lithology England",
    palette = "Set3",
    legend.show = TRUE,
    border.col = NA,           # quitar bordes de polígonos
    lwd = 0                    # grosor de línea 0
  ) +
  tm_layout(
    main.title = "Lithology England",
    main.title.position = "center",
    legend.outside = TRUE,               # leyenda FUERA del mapa
    legend.outside.position = "right",   # a la derecha
    legend.text.size = 0.6,
    legend.title.size = 0.8,
    legend.bg.color = "white",
    legend.bg.alpha = 0.9,
    legend.frame = TRUE,
    frame = FALSE,
    inner.margins = c(0.02, 0.02, 0.02, 0.02)
  )
#save lithology map
tmap_save(
  tm_shape(geology_england) +
    tm_polygons(
      "ROCK_D",
      title = "Lithology England",
      palette = "Set3",
      legend.show = TRUE,
      border.col = NA
    ) +
    tm_layout(
      legend.outside = TRUE,
      legend.outside.position = "right",
      legend.text.size = 0.7,
      legend.title.size = 0.9,
      legend.bg.color = "white",
      legend.bg.alpha = 1,
      frame = FALSE
    ),
  filename = "C:/Temp_R/lithology_map.png",
  width = 12,
  height = 8,
  dpi = 300
)
#_____________________________

#solar irradiation plot
library(terra)
library(tmap)

# Cargar el raster
solar_irradiation <- rast("C:/Temp_R/solar_irradiation_england.tif")

# Crear 10 categorías de igual tamaño
breaks <- seq(616, 1164, length.out = 7)

# Crear tabla de clasificación
m <- cbind(breaks[-length(breaks)], breaks[-1], 1:10)

solar_cat <- classify(solar_irradiation, m)

# Etiquetas
labels <- paste0(round(breaks[-length(breaks)]), " - ", round(breaks[-1]))

# Plot
tm_shape(solar_cat) +
  tm_raster(
    palette = "YlOrRd",
    labels = labels,
    title = "Solar Irradiation (kWh/m²)",
    legend.format = list(scientific = FALSE)
  ) +
  tm_layout(
    main.title = "Solar Irradiation Across England",
    main.title.position = "center",
    legend.outside = TRUE,
    legend.show = TRUE,
    legend.text.size = 0.7
  )

#save irradiation map
tmap_save(
  tm_shape(solar_cat) +
    tm_raster(
      palette = "YlOrRd",
      labels = labels,
      title = "Solar Irradiation (kWh/m²)",
      legend.show = TRUE
    ) +
    tm_layout(
      legend.outside = TRUE,
      legend.outside.position = "right",
      legend.text.size = 0.7,
      legend.title.size = 0.9,
      legend.bg.color = "white",
      legend.bg.alpha = 1,
      frame = FALSE,
      main.title = "Solar Irradiation Across England",
      main.title.size = 1.2
    ),
  filename = "C:/Temp_R/solar_irradiation_map.png",
  width = 12,
  height = 8,
  dpi = 300
)


#flood_risk

# Save flood risk map
tmap_save(
  tm_shape(flood_risk) +
    tm_polygons(
      col = "red",          # color único para las zonas FZ3
      alpha = 0.6,          # transparencia para ver el fondo si agregas capas
      border.col = NA,
      title = "Flood Risk Zone 3"
    ) +
    tm_layout(
      legend.show = FALSE,                  # no es necesario mostrar leyenda
      legend.outside = TRUE,
      legend.outside.position = "right",
      legend.text.size = 0.7,
      legend.title.size = 0.9,
      legend.bg.color = "white",
      legend.bg.alpha = 1,
      frame = FALSE,
      main.title = "Flood Risk Zone 3 in England",
      main.title.size = 1.2
    ),
  filename = "C:/Temp_R/flood_risk_zone3_map.png",
  width = 12,
  height = 8,
  dpi = 300
)


#______________________________________________________________________
#Flood risk option 2
library(sf)
library(tmap)

# Cargar los datos
flood_risk_1 <- st_read("C:/Temp_R/Flood_Risk_Areas.shp")

# Mapa con basemap
tm_shape(flood_risk_1) +
  tm_tiles("CartoDB.Positron") +       # ← Basemap profesional, no requiere ningún paquete extra
  tm_polygons(
    col = "lightblue",
    border.col = "darkblue",
    lwd = 1.5,
    alpha = 0.6
  ) +
  tm_compass(position = c("left", "top")) +
  tm_scale_bar(position = c("left", "bottom")) +
  tm_layout(
    main.title = "Flood Risk Areas (Zone 3)",
    main.title.size = 1.2,
    main.title.position = "center",
    legend.show = FALSE,
    frame = TRUE
  )

#save flood risk option 2
# Guardar el mapa con el mismo formato que los anteriores
tmap_save(
  tm_tiles("CartoDB.Positron") +   # basemap
    tm_shape(flood_risk_1) +
    tm_polygons(
      col = "lightblue",
      border.col = "darkblue",
      lwd = 1.5,
      alpha = 0.6
    ) +
    tm_compass(position = c("left", "top")) +
    tm_scale_bar(position = c("left", "bottom")) +
    tm_layout(
      main.title = "Flood Risk Areas",
      main.title.size = 1.2,
      main.title.position = "center",
      legend.show = FALSE,
      legend.outside = FALSE,
      frame = FALSE
    ),
  filename = "C:/Temp_R/flood_risk_option_2_map.png",
  width = 12,
  height = 8,
  dpi = 300
)
#_______________________________________________________

print("=== DATA CENTERS ===")
print(paste("Total DCs:", nrow(dc_england)))
print(head(dc_england))

print("=== GEOLOGY ===")
print(paste("Total features:", nrow(geology_england)))
print(names(geology_england))

print("=== FLOOD RISK ===")
print(paste("Total features:", nrow(flood_risk)))
print(names(flood_risk))

print("=== SOLAR IRRADIATION ===")
print(solar_irradiation)