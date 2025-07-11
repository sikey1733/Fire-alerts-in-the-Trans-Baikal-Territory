# Функция строит карту с ближайшим пожаром, населённым пунктом и водоёмом и сохраняет в файл
plot_nearest_fire_map <- function(fires_sf, places_sf, water_sf, output_path = "output/nearest_fire_map_ggplot.png") {
  required_packages <- c("ggplot2", "sf", "dplyr", "ggspatial", "prettymapr")
  for (pkg in required_packages) {
    if (!requireNamespace(pkg, quietly = TRUE)) install.packages(pkg)
    library(pkg, character.only = TRUE)
  }

  if (is.null(fires_sf) || nrow(fires_sf) == 0) return(NULL)
  if (is.null(places_sf) || nrow(places_sf) == 0) return(NULL)
  if (is.null(water_sf) || nrow(water_sf) == 0) return(NULL)

  nearest_fire <- fires_sf %>%
    filter(distance_to_settlement_km == min(distance_to_settlement_km, na.rm = TRUE)) %>%
    slice(1)

  nearest_place_name <- nearest_fire$settlement_name
  nearest_place <- places_sf %>% filter(name == nearest_place_name)
  if (nrow(nearest_place) == 0) return(NULL)

  fire_geom <- st_geometry(nearest_fire)
  water_sf$dist_to_fire <- as.numeric(st_distance(water_sf, fire_geom))
  nearest_water <- water_sf[which.min(water_sf$dist_to_fire), ]

  bbox <- st_bbox(nearest_fire)
  expand_factor <- 0.2
  lon_min <- bbox["xmin"] - expand_factor
  lon_max <- bbox["xmax"] + expand_factor
  lat_min <- bbox["ymin"] - expand_factor
  lat_max <- bbox["ymax"] + expand_factor

  p <- ggplot() +
    ggspatial::annotation_map_tile(type = "cartodbdark", zoomin = -1) +
    geom_sf(data = nearest_fire, color = "red", size = 4, shape = 8) +
    geom_sf(data = nearest_place, color = "blue", size = 3) +
    geom_sf(data = st_centroid(nearest_water), color = "cyan", size = 3) +
    coord_sf(xlim = c(lon_min, lon_max), ylim = c(lat_min, lat_max), expand = FALSE) +
    labs(
      title = "🔥 Ближайший пожар и водоём",
      caption = paste0(
        "📍 Населённый пункт: ", nearest_place_name,
        "\n💧 Водоём на расстоянии: ", round(nearest_fire$distance_to_water_km, 2), " км"
      )
    ) +
    theme_void() +
    theme(
      plot.title = element_text(size = 16, face = "bold", color = "white"),
      plot.caption = element_text(size = 12, color = "white"),
      panel.background = element_rect(fill = "black")
    )

  dir.create(dirname(output_path), showWarnings = FALSE, recursive = TRUE)
  ggsave(output_path, plot = p, width = 8, height = 6, dpi = 300, bg = "black")
  message("✅ Карта сохранена: ", output_path)
  return(p)
}
