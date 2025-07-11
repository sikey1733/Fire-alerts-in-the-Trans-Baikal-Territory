# Функция рассчитывает расстояния от пожаров до ближайших населённых пунктов и водоёмов
calculate_fire_distances <- function(fire_sf,
                                     region_names,
                                     places_cache = "data/places.gpkg",
                                     water_cache = "data/waterbodies.gpkg") {
  
  # Проверяет, что данные пожаров существуют и не пусты
  if (is.null(fire_sf) || nrow(fire_sf) == 0) {
    message("❌ Нет данных о пожарах для расчёта расстояний.")
    return(NULL)
  }
  
  # Проверяет, что входной объект является sf-объектом
  if (!inherits(fire_sf, "sf")) {
    stop("❌ Входной аргумент fire_sf должен быть sf-объектом.")
  }
  
  # Загружает населённые пункты (с использованием кэша или запроса)
  places_sf <- get_all_places(region_names, save_path = places_cache)
  if (is.null(places_sf) || nrow(places_sf) == 0) {
    message("❌ Не удалось загрузить населённые пункты.")
    return(NULL)
  }
  
  # Загружает водоёмы (с использованием кэша или запроса)
  water_sf <- get_all_waterbodies(region_names, save_path = water_cache)
  if (is.null(water_sf) || nrow(water_sf) == 0) {
    message("❌ Не удалось загрузить водоёмы.")
    return(NULL)
  }
  
  # Вычисляет центроиды полигонов водоёмов для удобства расчёта расстояний
  water_points <- st_centroid(water_sf)
  
  # Рассчитывает матрицу расстояний от каждого пожара до каждого населённого пункта
  dist_places <- st_distance(fire_sf, places_sf)
  
  # Находит индекс ближайшего населённого пункта для каждой точки пожара
  nearest_place_indices <- apply(dist_places, 1, which.min)
  
  # Находит минимальное расстояние до населённого пункта для каждой точки пожара
  min_place_dists <- apply(dist_places, 1, min)
  
  # Получает названия ближайших населённых пунктов по индексам
  nearest_place_names <- places_sf$name[nearest_place_indices]
  
  # Получает название региона ближайшего населённого пункта, если есть
  if ("region_name" %in% colnames(places_sf)) {
    nearest_region_names <- places_sf$region_name[nearest_place_indices]
  } else if ("addr.region" %in% colnames(places_sf)) {
    nearest_region_names <- places_sf$`addr.region`[nearest_place_indices]
  } else {
    nearest_region_names <- NA_character_
  }
  
  # Рассчитывает матрицу расстояний от пожаров до центроидов водоёмов
  dist_water <- st_distance(fire_sf, water_points)
  
  # Находит минимальное расстояние до водоёма для каждой точки пожара
  min_water_dists <- apply(dist_water, 1, min)
  
  # Добавляет колонки с расстояниями (в км), названиями населённых пунктов и регионов в sf-объект пожаров
  fire_sf$distance_to_settlement_km <- round(as.numeric(min_place_dists) / 1000, 2)
  fire_sf$settlement_name <- nearest_place_names
  fire_sf$settlement_region <- nearest_region_names
  fire_sf$distance_to_water_km <- round(as.numeric(min_water_dists) / 1000, 2)
  
  # Выводит сообщение об успешном завершении и количестве обработанных точек
  message("✅ Готово! Найдено точек: ", nrow(fire_sf))
  
  # Возвращает обновлённый sf-объект с дополнительными столбцами
  return(fire_sf)
}
