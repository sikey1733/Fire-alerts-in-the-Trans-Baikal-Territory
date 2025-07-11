# Функция строит карту с ближайшим пожаром, населённым пунктом и водоёмом и сохраняет в файл
plot_nearest_fire_map <- function(fires_sf, places_sf, water_sf, output_path = "output/nearest_fire_map_ggplot.png") {
  # Загружаем необходимые пакеты
  required_packages <- c("ggplot2", "sf", "dplyr", "ggmap")
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
  
  # Ближайший пожар к населённому пункту
  nearest_fire <- fires_sf %>%
    filter(distance_to_settlement_km == min(distance_to_settlement_km, na.rm = TRUE)) %>%
    slice(1)
  
  nearest_place_name <- nearest_fire$settlement_name
  nearest_place <- places_sf %>% filter(name == nearest_place_name)
  if (nrow(nearest_place) == 0) {
    message("❌ Не найден ближайший населённый пункт: ", nearest_place_name)
    return(NULL)
  }
  
  # Ближайший водоём к пожару
  fire_geom <- st_geometry(nearest_fire)
  water_sf$dist_to_fire <- as.numeric(st_distance(water_sf, fire_geom))
  nearest_water <- water_sf[which.min(water_sf$dist_to_fire), ]
  
  # Центр и масштаб карты (центрируем на пожаре)
  fire_coords <- st_coordinates(nearest_fire)
  center_lon <- fire_coords[1]
  center_lat <- fire_coords[2]
  
  # Получаем карту CartoDB Dark Matter с ggmap
  map_bg <- ggmap::get_map(location = c(lon = center_lon, lat = center_lat),
                           zoom = 10,
                           source = "cartodb",
                           maptype = "dark_all")
  
  # Подготовка координат для точек
  fire_df <- as.data.frame(fire_coords)
  colnames(fire_df) <- c("lon", "lat")
  
  place_coords <- st_coordinates(nearest_place) %>% as.data.frame()
  colnames(place_coords) <- c("lon", "lat")
  
  water_coords <- st_coordinates(st_centroid(nearest_water)) %>% as.data.frame()
  colnames(water_coords) <- c("lon", "lat")
  
  # Строим карту с точками и подписями
  p <- ggmap(map_bg) +
    geom_point(data = fire_df, aes(x = lon, y = lat), color = "red", size = 5, shape = 8) +
    geom_point(data = place_coords, aes(x = lon, y = lat), color = "blue", size = 4) +
    geom_point(data = water_coords, aes(x = lon, y = lat), color = "cyan", size = 4) +
    geom_text(data = place_coords, aes(x = lon, y = lat, label = nearest_place_name),
              color = "blue", vjust = -1, size = 5) +
    labs(
      title = "🔥 Ближайший пожар и водоём",
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
  
  # Создаем директорию и сохраняем
  dir.create(dirname(output_path), showWarnings = FALSE, recursive = TRUE)
  ggsave(output_path, plot = p, width = 8, height = 6, dpi = 300)
  message("✅ Карта сохранена: ", output_path)
  
  return(p)
}
