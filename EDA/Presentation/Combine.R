# This file combines the Fields data from FieldV2, watercourse, and data center.

uk_l2_combined <- uk_l2_web %>%
  left_join(
    uk_l2_with_water %>%
      st_drop_geometry() %>%
      select(
        shapeName, total_length, avg_length, total_area_km2, length_unit, link_count
      ),
    by = "shapeName"
  )%>%
  select(
    shapeName, area_km2, fields_total, geometry, total_length,
    avg_length, total_area_km2, length_unit, link_count
  )

dc_sf <- st_transform(dc_sf, st_crs(uk_l2_combined))
dc_joined <- st_join(dc_sf, uk_l2_combined, join = st_within)
dc_counts <- dc_joined %>%
  st_drop_geometry() %>%
  group_by(shapeName) %>%
  summarise(dc_count = n(), .groups = "drop")

uk_l2_with_dc <- uk_l2_combined %>%
  left_join(dc_counts, by = "shapeName") %>%
  mutate(dc_count = ifelse(is.na(dc_count), 0, dc_count))

uk_l2_with_dc%>%
  filter(fields_total>0)%>%
  ggplot()+
  # geom_point(aes(x=fields_total, y=length_unit, size=dc_count, color=area_km2))+
  geom_point(aes(x=fields_total, y=length_unit))+
  geom_text(
    # data = ~filter(.x, dc_count > 0),
    aes(x=fields_total, y=length_unit, label=shapeName),
    check_overlap = TRUE,
    vjust = -1,
    hjust = 0.4,
    size = 4
  )+
  labs(
    title = "Relationship between Number of Fields and Average
    Length of Watercourse in UK Level 2 Areas",
    x = "Number of Fields (brownfield)",
    y = "Average Length of Watercourse (length unit)",
    # size = "Num of DCs",
    # color = "Area (km2)"
  )+
  theme_minimal()

