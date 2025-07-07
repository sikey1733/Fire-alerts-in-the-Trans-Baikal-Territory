# Функция расчета расстояния между точками возгорания и населенными пунктами
calculate_fire_distances <- function(csv_data_dir = "data/",
                                     places_cache = "data/places.gpkg",
                                     water_cache = "data/waterbodies.gpkg") {
  fire_df <- read_data_viirs(csv_data_dir)
  if (is.null(fire_df)) {
    message("Нет данных о пожарах.")
    return(NULL)
  }
  
  fires_sf <- st_as_sf(fire_df, coords = c("longitude", "latitude"), crs = 4326)
  
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
  
  fire_coords <- st_coordinates(fires_sf)
  place_coords <- st_coordinates(places_sf)
  water_coords <- st_coordinates(st_centroid(water_sf))
  
  min_place_dists <- numeric(nrow(fires_sf))
  nearest_place_names <- character(nrow(fires_sf))
  min_water_dists <- numeric(nrow(fires_sf))
  
  for (i in seq_len(nrow(fire_coords))) {
    d_place <- geosphere::distHaversine(fire_coords[i, ], place_coords)
    d_water <- geosphere::distHaversine(fire_coords[i, ], water_coords)
    
    min_place_dists[i] <- min(d_place)
    nearest_place_names[i] <- places_sf$name[which.min(d_place)]
    min_water_dists[i] <- min(d_water)
  }
  
  fires_sf$distance_to_settlement_km <- round(min_place_dists / 1000, 2)
  fires_sf$settlement_name <- nearest_place_names
  fires_sf$distance_to_water_km <- round(min_water_dists / 1000, 2)
  
  message("Готово! Найдено точек: ", nrow(fires_sf))
  return(fires_sf)
}