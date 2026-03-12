small_decorations <- function() {
  tm_basemap("CartoDB.Positron") +
    tm_compass(position = c("right", "top")) +
    tm_scale_bar(position = c("right", "bottom"))
}

add_map_decorations <- function() {
  small_decorations()+
    tm_layout(
      main.title.size = 1,
      legend.outside = FALSE,
      legend.position = c("left", "top"),
      legend.bg.color = "white",
      legend.bg.alpha = 0.5,
      legend.frame = TRUE
    )
}
