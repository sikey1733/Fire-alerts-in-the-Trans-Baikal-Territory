# Функция строит карту с ближайшим пожаром, населённым пунктом и водоёмом и сохраняет в файл
plot_nearest_fire_map <- function(fires_sf, places_sf, water_sf, output_path = "output/nearest_fire_map_ggplot.png") {
  required_packages <- c("ggplot2", "sf", "dplyr", "maptiles", "grid", "terra")
  for (pkg in required_packages) {
    if (!requireNamespace(pkg, quietly = TRUE)) install.packages(pkg)
    library(pkg, character.only = TRUE)
  }

  if (is.null(fires_sf) || nrow(fires_sf) == 0) return(message("❌ Нет данных о пожарах"))
  if (is.null(places_sf) || nrow(places_sf) == 0) return(message("❌ Нет данных о населённых пунктах"))
  if (is.null(water_sf) || nrow(water_sf) == 0) return(message("❌ Нет данных о водоёмах"))

  nearest_fire <- fires_sf %>%
    filter(distance_to_settlement_km == min(distance_to_settlement_km, na.rm = TRUE)) %>%
    slice(1)

  nearest_place_name <- nearest_fire$settlement_name
  nearest_place <- places_sf %>% filter(name == nearest_place_name)
  if (nrow(nearest_place) == 0) return(message("❌ Не найден ближайший населённый пункт: ", nearest_place_name))

  fire_geom <- st_geometry(nearest_fire)
  water_sf$dist_to_fire <- as.numeric(st_distance(water_sf, fire_geom))
  nearest_water <- water_sf[which.min(water_sf$dist_to_fire), ]

  bbox <- st_bbox(nearest_fire)
  expand_factor <- 0.1
  lon_min <- max(-180, bbox["xmin"] - expand_factor)
  lon_max <- min(180, bbox["xmax"] + expand_factor)
  lat_min <- max(-85, bbox["ymin"] - expand_factor)
  lat_max <- min(85, bbox["ymax"] + expand_factor)

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

  # 🔧 Исправление — обработка RGB
  tile_array <- terra::as.array(tiles_raster)
  if (length(dim(tile_array)) == 3 && dim(tile_array)[3] == 3) {
    tile_array <- tile_array / 255
    tile_array <- aperm(tile_array, c(2, 1, 3))  # [x, y, c] → [y, x, c]
    tiles_grob <- grid::rasterGrob(tile_array,
                                   width = unit(1, "npc"),
                                   height = unit(1, "npc"),
                                   interpolate = TRUE)
  } else {
    stop("❌ Ошибка: ожидался RGB-растровый слой.")
  }

  fire_coords <- st_coordinates(nearest_fire) %>% as.data.frame()
  place_coords <- st_coordinates(nearest_place) %>% as.data.frame()
  water_coords <- st_coordinates(st_centroid(nearest_water)) %>% as.data.frame()

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

  dir.create(dirname(output_path), showWarnings = FALSE, recursive = TRUE)
  ggsave(output_path, plot = p, width = 8, height = 6, dpi = 300)
  message("✅ Карта сохранена: ", output_path)
  return(p)
}
