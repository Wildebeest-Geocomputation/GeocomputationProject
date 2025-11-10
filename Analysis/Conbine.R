uk_l2_web
uk_l2_with_water

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
    shapeName, area_km2, fields_total, geometry, total_length, avg_length, total_area_km2, length_unit, link_count
  )

uk_l2_combined%>%
  filter(fields_total>0)%>%
  ggplot()+
  geom_point(aes(x=fields_total, y=length_unit))+
  geom_text(
    aes(x=fields_total, y=length_unit, label=shapeName),
    check_overlap = TRUE,
    vjust = -1,
    hjust = 0.4,
    size = 3
  )+
  labs(
    title = "Relationship between Number of Fields and Average Length of Watercourse in UK Level 2 Areas",
    x = "Number of Fields (brownfield)",
    y = "Average Length of Watercourse (length unit)"
  )+
  theme_minimal()

