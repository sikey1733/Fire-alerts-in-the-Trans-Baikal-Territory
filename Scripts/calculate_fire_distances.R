# Расчёт расстояний до ближайшего населённого пункта и водоёма
calculate_fire_distances <- function(fire_sf,
                                     region_names,
                                     places_cache = "data/places.gpkg",
                                     water_cache = "data/waterbodies.gpkg") {

  if (is.null(fire_sf) || nrow(fire_sf) == 0) {
    message("❌ Нет данных о пожарах для расчёта расстояний.")
    return(NULL)
  }

  # Убедимся, что это sf-объект
  if (!inherits(fire_sf, "sf")) {
    stop("❌ Входной аргумент fire_sf должен быть sf-объектом.")
  }

  # Загружаем населённые пункты
  places_sf <- get_all_places(region_names, save_path = places_cache)
  if (is.null(places_sf) || nrow(places_sf) == 0) {
    message("❌ Не удалось загрузить населённые пункты.")
    return(NULL)
  }

  # Загружаем водоёмы
  water_sf <- get_all_waterbodies(region_names, save_path = water_cache)
  if (is.null(water_sf) || nrow(water_sf) == 0) {
    message("❌ Не удалось загрузить водоёмы.")
    return(NULL)
  }

  # Центроиды водоёмов
  water_points <- st_centroid(water_sf)

  # Расчёт расстояний до населённых пунктов
  dist_places <- st_distance(fire_sf, places_sf)
  nearest_place_indices <- apply(dist_places, 1, which.min)
  min_place_dists <- apply(dist_places, 1, min)

  # Названия и регионы ближайших населённых пунктов
  nearest_place_names <- places_sf$name[nearest_place_indices]
  if ("region_name" %in% colnames(places_sf)) {
    nearest_region_names <- places_sf$region_name[nearest_place_indices]
  } else if ("addr.region" %in% colnames(places_sf)) {
    nearest_region_names <- places_sf$`addr.region`[nearest_place_indices]
  } else {
    nearest_region_names <- NA_character_
  }

  # Расчёт расстояний до ближайших водоёмов
  dist_water <- st_distance(fire_sf, water_points)
  min_water_dists <- apply(dist_water, 1, min)

  # Добавление колонок к sf
  fire_sf$distance_to_settlement_km <- round(as.numeric(min_place_dists) / 1000, 2)
  fire_sf$settlement_name <- nearest_place_names
  fire_sf$settlement_region <- nearest_region_names
  fire_sf$distance_to_water_km <- round(as.numeric(min_water_dists) / 1000, 2)

  message("✅ Готово! Найдено точек: ", nrow(fire_sf))
  return(fire_sf)
}
