# Функция фильтрует данные пожаров по указанным регионам из полигона
filter_fires_by_region <- function(csv_data_dir = "data/",
                                   polygon_path = "data/polygons.gpkg", 
                                   region_names = c("Забайкальский край, Россия", 
                                                    "Республика Бурятия, Россия", 
                                                    "Амурская область, Россия", 
                                                    "Иркутская область, Россия")) {
  
  # Читает данные пожаров из CSV (использует функцию read_data_viirs)
  fire_df <- read_data_viirs(csv_data_dir)
  if (is.null(fire_df)) {
    message("Ошибка: данные о пожарах не найдены!")
    return(NULL)
  }
  
  # Фильтрует записи по уровню доверия: только "n" (normal) и "h" (high)
  fire_df <- fire_df %>% filter(confidence %in% c("n", "h"))
  
  # Проверяет, остались ли данные после фильтрации
  if (nrow(fire_df) == 0) {
    message("Нет точек с высоким или нормальным доверием.")
    return(NULL)
  }
  
  # Получает полигоны указанных регионов (функция request_polygon)
  region_poly <- request_polygon(region_names, save_path = polygon_path)
  if (is.null(region_poly)) {
    message("Ошибка: не удалось получить полигоны для указанных регионов.")
    return(NULL)
  }
  
  message("Полигоны получены и готовы для фильтрации!")
  
  # Преобразует таблицу пожаров в spatial-объект sf с координатами lon/lat
  fires_sf <- st_as_sf(fire_df, coords = c("longitude", "latitude"), crs = 4326)
  
  # Проверяет, какие точки находятся внутри полигонов регионов
  message("Фильтрация точек по полигонам...")
  inside <- st_within(fires_sf, region_poly, sparse = FALSE)
  
  # Оставляет только точки, которые попадают хотя бы в один из регионов
  filtered <- fires_sf[apply(inside, 1, any), ]
  
  # Проверяет, есть ли точки внутри регионов после фильтрации
  if (nrow(filtered) == 0) {
    message("Не найдено точек внутри указанных регионов.")
    return(NULL)
  }
  
  # Сообщает сколько точек найдено и возвращает отфильтрованный набор
  message("За прошлый день найдено ", nrow(filtered), " точек внутри указанных регионов.")
  
  return(filtered)
}
