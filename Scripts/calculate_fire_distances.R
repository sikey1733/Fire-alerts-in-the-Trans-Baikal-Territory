calculate_fire_distances <- function(csv_data_dir = "data/",
                                     places_cache = "data/places.gpkg",
                                     water_cache = "data/waterbodies.gpkg") {
  # Чтение данных о пожарах
  fire_df <- read_data_viirs(csv_data_dir)
  if (is.null(fire_df)) {
    message("Нет данных о пожарах.")
    return(NULL)
  }
  
  # Преобразование в sf объект
  fires_sf <- st_as_sf(fire_df, coords = c("longitude", "latitude"), crs = 4326)
  
  # Чтение данных о населенных пунктах и водоемах
  region_names <- c(
    "Забайкальский край, Россия",
    "Республика Бурятия, Россия",
    "Амурская область, Россия",
    "Иркутская область, Россия"
  )
  
  places_sf <- get_all_places(region_names, save_path = places_cache)
  if (is.null(places_sf)) {
    message("Не удалось загрузить населённые пункты.")
    return(NULL)
  }
  
  water_sf <- get_all_waterbodies(region_names, save_path = water_cache)
  if (is.null(water_sf)) {
    message("Не удалось загрузить водоёмы.")
    return(NULL)
  }
  
  # Расчёт расстояний
  fire_coords <- st_coordinates(fires_sf)
  place_coords <- st_coordinates(places_sf)
  water_coords <- st_coordinates(st_centroid(water_sf))
  
  # Инициализация векторов для минимальных расстояний
  min_place_dists <- numeric(nrow(fires_sf))
  nearest_place_names <- character(nrow(fires_sf))
  min_water_dists <- numeric(nrow(fires_sf))
  
  # Вычисление расстояний для каждого пожара
  for (i in seq_len(nrow(fire_coords))) {
    # Расстояние до ближайшего населённого пункта
    d_place <- distVincentySphere(fire_coords[i, ], place_coords)
    # Расстояние до ближайшего водоёма
    d_water <- distVincentySphere(fire_coords[i, ], water_coords)
    
    min_place_dists[i] <- min(d_place)
    nearest_place_names[i] <- places_sf$name[which.min(d_place)]
    min_water_dists[i] <- min(d_water)
  }
  
  # Добавляем новые данные в sf
  fires_sf$distance_to_settlement_km <- round(min_place_dists / 1000, 2)
  fires_sf$settlement_name <- nearest_place_names
  fires_sf$distance_to_water_km <- round(min_water_dists / 1000, 2)
  
  message("Готово! Найдено точек: ", nrow(fires_sf))
  
  # Возвращаем обработанные данные
  return(fires_sf)
}
