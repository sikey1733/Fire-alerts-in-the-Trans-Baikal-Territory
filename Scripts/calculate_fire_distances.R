calculate_fire_distances <- function(region_names,
                                     csv_data_dir = "data/",
                                     places_cache = "data/places.gpkg",
                                     water_cache = "data/waterbodies.gpkg") {
  fire_df <- read_data_viirs(csv_data_dir)
  if (is.null(fire_df)) {
    message("Нет данных о пожарах.")
    return(NULL)
  }
  
  fires_sf <- st_as_sf(fire_df, coords = c("longitude", "latitude"), crs = 4326)
  
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
  
  # Преобразуем полигоны водоемов к центроидам
  water_points <- st_centroid(water_sf)
  
  # Расстояния
  dist_places <- st_distance(fires_sf, places_sf)
  dist_water <- st_distance(fires_sf, water_points)
  
  min_place_dists <- apply(dist_places, 1, min)
  nearest_place_indices <- apply(dist_places, 1, which.min)
  nearest_place_names <- places_sf$name[nearest_place_indices]
  
  # Добавляем region_name ближайшего населённого пункта
  nearest_region_names <- places_sf$region_name[nearest_place_indices]
  
  min_water_dists <- apply(dist_water, 1, min)
  
  fires_sf$distance_to_settlement_km <- round(as.numeric(min_place_dists) / 1000, 2)
  fires_sf$settlement_name <- nearest_place_names
  fires_sf$settlement_region <- nearest_region_names  # Новое поле с регионом
  fires_sf$distance_to_water_km <- round(as.numeric(min_water_dists) / 1000, 2)
  
  message("Готово! Найдено точек: ", nrow(fires_sf))
  
  return(fires_sf)
}
