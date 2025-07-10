# Функция для построения карты ближайшего пожара, населённого пункта и водоёма
plot_nearest_fire_map <- function(fires_sf, places_sf, water_sf, output_path = "output/nearest_fire_map_ggmap.png") {
  # Проверка и загрузка пакетов
  required_packages <- c("ggmap", "ggplot2", "sf", "dplyr")
  for (pkg in required_packages) {
    if (!requireNamespace(pkg, quietly = TRUE)) install.packages(pkg)
    library(pkg, character.only = TRUE)
  }

  # Проверка данных
  if (is.null(fires_sf) || nrow(fires_sf) == 0) {
    message("❌ Нет данных о пожарах")
    return(NULL)
  }
  if (is.null(places_sf) || nrow(places_sf) == 0) {
    message("❌ Нет данных о населённых пунктах")
    return(NULL)
  }
  if (is.null(water_sf) || nrow(water_sf) == 0) {
    message("❌ Нет данных о водоёмах")
    return(NULL)
  }

  # 1. Ближайший пожар
  nearest_fire <- fires_sf %>%
    filter(distance_to_settlement_km == min(distance_to_settlement_km, na.rm = TRUE)) %>%
    slice(1)

  nearest_place_name <- nearest_fire$settlement_name

  # 2. Ближайший населённый пункт
  nearest_place <- places_sf %>% filter(name == nearest_place_name)
  if (nrow(nearest_place) == 0) {
    message("❌ Не найден ближайший населённый пункт: ", nearest_place_name)
    return(NULL)
  }

  # 3. Ближайший водоём
  fire_geom <- st_geometry(nearest_fire)
  water_sf$dist_to_fire <- as.numeric(st_distance(water_sf, fire_geom))
  nearest_water <- water_sf[which.min(water_sf$dist_to_fire), ]

  # 4. Расширенный bbox с проверками
  bbox <- st_bbox(nearest_fire)
  expand_factor <- 0.1
  lon_min <- max(-180, bbox["xmin"] - expand_factor)
  lon_max <- min(180, bbox["xmax"] + expand_factor)
  lat_min <- max(-85, bbox["ymin"] - expand_factor)
  lat_max <- min(85, bbox["ymax"] + expand_factor)

  # Минимальное расширение bbox, чтобы не было нулевого размера
  if ((lon_max - lon_min) < 0.01) {
    lon_min <- lon_min - 0.01
    lon_max <- lon_max + 0.01
  }
  if ((lat_max - lat_min) < 0.01) {
    lat_min <- lat_min - 0.01
    lat_max <- lat_max + 0.01
  }

  message(sprintf("BBox: left=%.4f, bottom=%.4f, right=%.4f, top=%.4f", lon_min, lat_min, lon_max, lat_max))

  # 5. Получение тайлов OpenStreetMap через get_map (без API-ключей)
  basemap <- get_map(
    location = c(left = lon_min, bottom = lat_min, right = lon_max, top = lat_max),
    source = "osm",
    zoom = 10
  )

  # 6. Координаты
  fire_coords <- st_coordinates(nearest_fire) %>% as.data.frame()
  place_coords <- st_coordinates(nearest_place) %>% as.data.frame()
  water_coords <- st_coordinates(st_centroid(nearest_water)) %>% as.data.frame()

  # 7. Построение карты
  p <- ggmap(basemap) +
    geom_point(data = fire_coords, aes(X, Y), color = "red", size = 4, shape = 8) +
    geom_point(data = place_coords, aes(X, Y), color = "blue", size = 3) +
    geom_point(data = water_coords, aes(X, Y), color = "cyan", size = 3) +
    geom_text(data = place_coords, aes(X, Y, label = nearest_place_name),
              color = "blue", vjust = -1.5, size = 5) +
    labs(
      title = "🔥 Ближайший пожар, населённый пункт и водоём",
      caption = paste0(
        "📍 Населённый пункт: ", nearest_place_name,
        "\n💧 Водоём на расстоянии: ", round(nearest_fire$distance_to_water_km, 2), " км"
      )
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 16, face = "bold"),
      plot.caption = element_text(size = 12)
    )

  # 8. Сохранение
  dir.create(dirname(output_path), showWarnings = FALSE, recursive = TRUE)
  ggsave(output_path, plot = p, width = 8, height = 6, dpi = 300)
  message("✅ Карта сохранена: ", output_path)

  return(p)
}
