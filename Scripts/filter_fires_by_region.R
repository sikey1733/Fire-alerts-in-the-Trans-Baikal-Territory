filter_fires_by_region <- function(csv_data_dir = "data/",
                                   polygon_path = "data/polygons.gpkg", 
                                   region_names = c("Забайкальский край, Россия", 
                                                    "Республика Бурятия, Россия", 
                                                    "Амурская область, Россия", 
                                                    "Иркутская область, Россия")) {
  
  # Шаг 1: Чтение данных
  fire_df <- read_data_viirs(csv_data_dir)
  if (is.null(fire_df)) {
    message("Ошибка: данные о пожарах не найдены!")
    return(NULL)
  }
  
  # Шаг 2: Фильтрация данных по уровню доверия
  fire_df <- fire_df %>% filter(confidence %in% c("n", "h", "l"))
  
  if (nrow(fire_df) == 0) {
    message("Нет точек с высоким или нормальным доверием.")
    return(NULL)
  }
  
  # Шаг 3: Получение полигонов для регионов
  region_poly <- request_polygon(region_names, save_path = polygon_path)
  if (is.null(region_poly)) {
    message("Ошибка: не удалось получить полигоны для указанных регионов.")
    return(NULL)
  }
  
  message("Полигоны получены и готовы для фильтрации!")
  
  # Шаг 4: Преобразование данных в sf-объект
  fires_sf <- st_as_sf(fire_df, coords = c("longitude", "latitude"), crs = 4326)
  
  # Шаг 5: Фильтрация точек внутри полигонов
  message("Фильтрация точек по полигонам...")
  inside <- st_within(fires_sf, region_poly, sparse = FALSE)
  filtered <- fires_sf[apply(inside, 1, any), ]
  
  if (nrow(filtered) == 0) {
    message("Не найдено точек внутри указанных регионов.")
    return(NULL)
  }
  
  message("За прошлый день найдено ", nrow(filtered), " точек внутри указанных регионов.")
  
  return(filtered)
}
