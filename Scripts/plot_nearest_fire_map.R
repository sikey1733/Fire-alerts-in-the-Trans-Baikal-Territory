# Функция строит карту с ближайшим пожаром, населённым пунктом и водоёмом и сохраняет в файл
plot_nearest_fire_map <- function(fires_sf, places_sf, water_sf, output_path = "output/nearest_fire_map_ggplot.png") {
  # Загружает необходимые пакеты, устанавливая при отсутствии
  required_packages <- c("ggplot2", "sf", "dplyr", "maptiles", "grid", "terra", "raster")
  for (pkg in required_packages) {
    if (!requireNamespace(pkg, quietly = TRUE)) install.packages(pkg)
    library(pkg, character.only = TRUE)
  }

  # Проверяет, что данные пожаров существуют и не пусты
  if (is.null(fires_sf) || nrow(fires_sf) == 0) {
    message("❌ Нет данных о пожарах")
    return(NULL)
  }
  # Проверяет наличие данных населённых пунктов
  if (is.null(places_sf) || nrow(places_sf) == 0) {
    message("❌ Нет данных о населённых пунктах")
    return(NULL)
  }
  # Проверяет наличие данных водоёмов
  if (is.null(water_sf) || nrow(water_sf) == 0) {
    message("❌ Нет данных о водоёмах")
    return(NULL)
  }

  # Определяет ближайший пожар по минимальному расстоянию до населённого пункта
  nearest_fire <- fires_sf %>%
    filter(distance_to_settlement_km == min(distance_to_settlement_km, na.rm = TRUE)) %>%
    slice(1)

  nearest_place_name <- nearest_fire$settlement_name
  nearest_place <- places_sf %>% filter(name == nearest_place_name)
  if (nrow(nearest_place) == 0) {
    message("❌ Не найден ближайший населённый пункт: ", nearest_place_name)
    return(NULL)
  }

  # Рассчитывает расстояния от водоёмов до ближайшего пожара и выбирает ближайший
  fire_geom <- st_geometry(nearest_fire)
  water_sf$dist_to_fire <- as.numeric(st_distance(water_sf, fire_geom))
  nearest_water <- water_sf[which.min(water_sf$dist_to_fire), ]

  # Определяет bounding box вокруг пожара с небольшим расширением
  bbox <- st_bbox(nearest_fire)
  expand_factor <- 0.1
  lon_min <- max(-180, bbox["xmin"] - expand_factor)
  lon_max <- min(180, bbox["xmax"] + expand_factor)
  lat_min <- max(-85, bbox["ymin"] - expand_factor)
  lat_max <- min(85, bbox["ymax"] + expand_factor)

  # Кэширование тайлов карты: загружает из файла или скачивает с OpenStreetMap
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

  # Преобразует растровый слой в grob для добавления в ggplot
  tiles_matrix <- as.matrix(raster::raster(tiles_raster[[1]]))
  tiles_grob <- grid::rasterGrob(
    tiles_matrix,
    width = unit(1, "npc"), height = unit(1, "npc"),
    interpolate = TRUE
  )

  # Получает координаты пожара, населённого пункта и водоёма
  fire_coords <- st_coordinates(nearest_fire) %>% as.data.frame()
  place_coords <- st_coordinates(nearest_place) %>% as.data.frame()
  water_coords <- st_coordinates(st_centroid(nearest_water)) %>% as.data.frame()

  # Строит карту с подложкой и точками пожара, населённого пункта и водоёма
  p <- ggplot() +
    annotation_custom(tiles_grob,
                      xmin = lon_min, xmax = lon_max,
                      ymin = lat_min, ymax = lat_max) +
    coord_sf(xlim = c(lon_min, lon_max), ylim = c(lat_min, lat_max), expand = FALSE) +
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

  # Создаёт папку для сохранения и сохраняет карту в файл
  dir.create(dirname(output_path), showWarnings = FALSE, recursive = TRUE)
  ggsave(output_path, plot = p, width = 8, height = 6, dpi = 300)
  message("✅ Карта сохранена: ", output_path)

  # Возвращает объект ggplot для дальнейшей работы или отображения
  return(p)
}
