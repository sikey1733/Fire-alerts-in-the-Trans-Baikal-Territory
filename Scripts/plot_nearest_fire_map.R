# Функция строит карту с ближайшим пожаром, населённым пунктом и водоёмом и сохраняет в файл
plot_nearest_fire_map <- function(fires_sf, places_sf, water_sf, output_path = "output/nearest_fire_map_ggplot.png") {
  # Устанавливаем и загружаем необходимые пакеты
  required_packages <- c("ggplot2", "sf", "dplyr", "terra", "ggspatial")
  for (pkg in required_packages) {
    if (!requireNamespace(pkg, quietly = TRUE)) install.packages(pkg)
    library(pkg, character.only = TRUE)
  }

  # Проверка входных данных
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

  # Определяем ближайший пожар
  nearest_fire <- fires_sf %>%
    filter(distance_to_settlement_km == min(distance_to_settlement_km, na.rm = TRUE)) %>%
    slice(1)

  nearest_place_name <- nearest_fire$settlement_name
  nearest_place <- places_sf %>% filter(name == nearest_place_name)
  if (nrow(nearest_place) == 0) {
    message("❌ Не найден ближайший населённый пункт: ", nearest_place_name)
    return(NULL)
  }

  # Определяем ближайший водоём
  fire_geom <- st_geometry(nearest_fire)
  water_sf$dist_to_fire <- as.numeric(st_distance(water_sf, fire_geom))
  nearest_water <- water_sf[which.min(water_sf$dist_to_fire), ]

  # Задаём bounding box с расширением
  bbox <- st_bbox(nearest_fire)
  expand_factor <- 0.1
  lon_min <- max(-180, bbox["xmin"] - expand_factor)
  lon_max <- min(180, bbox["xmax"] + expand_factor)
  lat_min <- max(-85, bbox["ymin"] - expand_factor)
  lat_max <- min(85, bbox["ymax"] + expand_factor)

  # Загружаем тайлы с OpenStreetMap и сохраняем кэш
  cache_file <- "data/maptiles_cache/tiles.tif"
  if (file.exists(cache_file)) {
    message("📦 Загружаю тайлы из кэша: ", cache_file)
    tiles_raster <- terra::rast(cache_file)
  } else {
    message("🌐 Загружаю тайлы с сервера OpenStreetMap...")
    tiles_raster <- maptiles::get_tiles(
      fires_sf,
      provider = "OpenStreetMap",
      zoom = 8,
      crop = FALSE
    )
    dir.create("data/maptiles_cache", showWarnings = FALSE, recursive = TRUE)
    terra::writeRaster(tiles_raster, cache_file, overwrite = TRUE)
    message("✅ Тайлы сохранены: ", cache_file)
  }

  # Убедимся, что CRS совпадают
  if (!terra::compareGeom(tiles_raster, vect(nearest_fire), stopOnError = FALSE)) {
    tiles_raster <- terra::project(tiles_raster, st_crs(nearest_fire)$wkt)
  }

  # Построение карты
  p <- ggplot() +
    layer_spatraster_rgb(data = tiles_raster) +
    geom_sf(data = nearest_fire, color = "red", size = 4, shape = 8) +
    geom_sf(data = nearest_place, color = "blue", size = 3) +
    geom_sf(data = st_centroid(nearest_water), color = "cyan", size = 3) +
    geom_text(data = st_coordinates(nearest_place) %>% as.data.frame(),
              aes(X, Y, label = nearest_place_name),
              color = "blue", vjust = -1.5, size = 5) +
    coord_sf(xlim = c(lon_min, lon_max), ylim = c(lat_min, lat_max), expand = FALSE) +
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

  # Сохраняем в файл
  dir.create(dirname(output_path), showWarnings = FALSE, recursive = TRUE)
  ggsave(output_path, plot = p, width = 8, height = 6, dpi = 300)
  message("✅ Карта сохранена: ", output_path)

  return(p)
}
