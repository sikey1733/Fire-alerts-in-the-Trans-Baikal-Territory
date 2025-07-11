# Функция запрашивает водоёмы из OSM для списка регионов и сохраняет в файл
get_all_waterbodies <- function(region_names, save_path = "data/waterbodies.gpkg") {
  
  # Если файл с водоёмами существует — загружает его
  if (file.exists(save_path)) {
    message("Загружаю водоёмы из файла: ", save_path)
    waterbodies <- st_read(save_path, quiet = TRUE)
    
    # Если есть колонка name — фильтрует объекты с кириллическими названиями
    if ("name" %in% names(waterbodies)) {
      waterbodies <- waterbodies[grepl("[А-Яа-яЁё]", waterbodies$name), ]
    }
    
    return(waterbodies)
  }
  
  # Если файла нет — делает запросы для каждого региона по ключу natural=water
  water_list <- lapply(region_names, function(name) {
    message("Запрашиваю водоёмы: ", name)
    
    # Формирует запрос к OSM с фильтром на водоёмы
    query <- opq(name) %>%
      add_osm_feature(key = "natural", value = "water")
    
    # Выполняет запрос, возвращает NULL при ошибке
    result <- tryCatch(osmdata_sf(query), error = function(e) return(NULL))
    
    # Если есть полигональные объекты — выбирает name и геометрию
    if (!is.null(result) && !is.null(result$osm_polygons)) {
      df <- result$osm_polygons %>%
        select(name, geometry) %>%
        st_transform(4326)
      
      # Фильтрует объекты с кириллическими названиями (если есть name)
      if ("name" %in% names(df)) {
        df <- df[grepl("[А-Яа-яЁё]", df$name), ]
      }
      
      return(df)
    }
    return(NULL)
  })
  
  # Удаляет пустые элементы списка результатов запросов
  water_list <- water_list[!sapply(water_list, is.null)]
  
  # Если данные не получены — выводит сообщение и возвращает NULL
  if (length(water_list) == 0) {
    message("Не удалось получить водоёмы.")
    return(NULL)
  }
  
  # Объединяет все результаты в один sf-объект
  combined <- bind_rows(water_list)
  
  # Создаёт директорию для сохранения, если её нет
  dir.create(dirname(save_path), showWarnings = FALSE, recursive = TRUE)
  
  # Сохраняет объединённые водоёмы в GeoPackage
  st_write(combined, save_path, delete_dsn = TRUE, quiet = TRUE)
  message("Водоёмы сохранены в: ", save_path)
  
  # Возвращает объединённый набор водоёмов
  return(combined)
}
