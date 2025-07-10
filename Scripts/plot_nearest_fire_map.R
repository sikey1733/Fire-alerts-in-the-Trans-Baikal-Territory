# Функция для построения карты ближайшего пожара, населённого пункта и водоёма
plot_nearest_fire_map <- function(fires_sf, places_sf, water_sf, output_path = "output/nearest_fire_map_ggplot.png") {
  if (!requireNamespace("ggrepel", quietly = TRUE)) {
    install.packages("ggrepel", repos = "https://cloud.r-project.org")
  }
  library(ggrepel)

  if (is.null(fires_sf) || nrow(fires_sf) == 0) {
    message("❌ Нет данных о пожарах")
    return(NULL)
  }

  # 1. Выбор ближайшего пожара
  nearest_fire <- fires_sf %>%
    filter(distance_to_settlement_km == min(distance_to_settlement_km, na.rm = TRUE)) %>%
    slice(1)

  nearest_place_name <- nearest_fire$settlement_name

  # 2. Получение ближайшего населённого пункта
  nearest_place <- places_sf %>% filter(name == nearest_place_name)
  if (nrow(nearest_place) == 0) {
    message("❌ Не найден ближайший населённый пункт для имени: ", nearest_place_name)
    return(NULL)
  }

  # 3. Получение ближайшего водоёма
  fire_geom <- st_geometry(nearest_fire)
  water_sf$dist_to_fire <- as.numeric(st_distance(water_sf, fire_geom))
  nearest_water <- water_sf[which.min(water_sf$dist_to_fire), ]

  # 4. Объединение объектов (необязательно, можно убрать, если не используешь)
  all_features <- rbind(
    st_sf(type = "Пожар", geometry = st_geometry(nearest_fire)),
    st_sf(type = "Населённый пункт", geometry = st_geometry(nearest_place)),
    st_sf(type = "Водоём", geometry = st_centroid(st_geometry(nearest_water)))
  )

  # 5. Построение карты
  place_coords <- st_coordinates(st_centroid(nearest_place)) %>%
    as.data.frame() %>%
    mutate(label = nearest_place_name)

  p <- ggplot() +
    geom_sf(data = nearest_water, fill = "cyan", color = "darkcyan", alpha = 0.4) +
    geom_sf(data = nearest_place, color = "blue", size = 3) +
    geom_sf(data = nearest_fire, color = "red", size = 3) +
    geom_text_repel(data = place_coords, aes(X, Y, label = label), color = "blue", size = 4) +
    annotation_scale(location = "bl", width_hint = 0.3) +
    annotation_north_arrow(location = "bl", which_north = "true", 
                           style = north_arrow_fancy_orienteering) +
    labs(
      title = "🔥 Ближайший пожар, населённый пункт и водоём",
      caption = paste0("📍 Населённый пункт: ", nearest_place_name,
                       "\n💧 Водоём: ", round(nearest_fire$distance_to_water_km, 2), " км")
    ) +
    theme_minimal()

  # 7. Сохранение карты
  dir.create(dirname(output_path), showWarnings = FALSE, recursive = TRUE)
  ggsave(output_path, plot = p, width = 8, height = 6, dpi = 300)
  message("✅ Карта сохранена: ", output_path)

  return(p)
}
