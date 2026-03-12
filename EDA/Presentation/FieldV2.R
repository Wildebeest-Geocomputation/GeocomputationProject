library(rnaturalearth)
library(rgeoboundaries)
# uk <- gb_adm0("United Kingdom")
uk_l2 <- gb_adm2("United Kingdom")
plot(st_geometry(uk_l2))

get_fields <- function(
    data, uk, pre = "green-belt"
  ){
  poly_ll <- data %>%
    # st_as_sf(wkt = "geometry", crs = 4326) %>%
    st_make_valid() %>%
    mutate(poly_id = dplyr::row_number())

  dsf <- st_transform(poly_ll, st_crs(uk))

  id_col <- 'shapeName'

  pts_in_poly <- st_join(
    dsf,
    uk[, id_col, drop=FALSE],
    join = st_intersects,
    left = FALSE
  )
  print(dim(pts_in_poly))

  counts_all <- pts_in_poly%>%
    filter(dataset == pre)%>%
    st_drop_geometry()%>%
    count(.data[[id_col]], prefix, name = "n")

  fieled_counts <- counts_all%>%
    group_by(.data[[id_col]])%>%
    summarise(fields_total = sum(n), .groups = "drop")

  uk_l2_counts <- uk%>%
    left_join(fieled_counts, by = setNames(id_col, id_col))%>%
    mutate(fields_total = tidyr::replace_na(fields_total, 0L))

  uk_l2_counts$area_km2 <- st_area(uk_l2_counts) %>%
    units::set_units("km^2") %>%
    as.numeric()

  uk_l2_web <- st_transform(uk_l2_counts, 4326)

  return(uk_l2_web)
}

data_sf$dataset%>%unique()

# "brownfield-site" "green-belt"
type <- "brownfield-site"

uk_l2_web <- get_fields(
  data = data_sf,
  uk = uk_l2,
  pre = type
)

# uk_l2_web <- uk_l2_web%>%filter(fields_total>0)
uk_l2_web%>%
  filter(fields_total>0)%>%
  ggplot()+
  geom_point(aes(x=fields_total, y=area_km2))+
  geom_text(
    aes(x=fields_total, y=area_km2, label=shapeName),
    check_overlap = TRUE,
    vjust = -1,
    hjust = 0.5,
    size = 3
  )+
  labs(
    title = paste("Fields count vs Area for", type),
    x = "Fields count",
    y = "Area (km^2)"
  )

uk_l2_web%>%
  filter(fields_total > 0)%>%
  st_drop_geometry()%>%
  ggplot()+
  geom_col(aes(x=reorder(shapeName, fields_total), y = fields_total, fill = fields_total))+
  coord_flip()

# uk_l2_web <- uk_l2_web%>%
#   mutate(
#     fields_per_km2 = fields_total / area_km2
#   )%>%
#   # fillna
#   mutate(
#     fields_per_km2 = tidyr::replace_na(fields_per_km2, 0)
#   )

library(tmap)
tmap_mode("view")
tm_basemap("CartoDB.Positron") +
  tm_shape(uk_l2_web) +
  tm_polygons(
    col = "fields_total",
    # style = "equal",
    palette = "viridis",
    border.col = "white",
    lwd = 0.3,
    title = paste(type, "count"),
  ) +
  tm_compass(position = c("right","top")) +
  tm_scale_bar(position = c("right","top")) +
  # add shapeName name one each county
  # tm_shape(uk_l2_web %>% filter(fields_total > 12))+
  tm_shape(uk_l2_web %>% filter(fields_total > 300))+
  tm_text(
    text = "shapeName",
    size = 1.5,
    col = "#eb5b8b",
    shadow = TRUE,
    remove.overlap = TRUE
  ) +
  tm_view()
