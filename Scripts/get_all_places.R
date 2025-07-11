# Функция запрашивает населённые пункты для списка регионов и сохраняет их в файл
get_all_places <- function(region_names, save_path = "data/places.gpkg") {
  
  # Если файл с населёнными пунктами уже существует — загружает его
  if (file.exists(save_path)) {
    message("Загружаю населённые пункты из файла: ", save_path)
    places <- sf::st_read(save_path, quiet = TRUE)
    
    # Фильтрует населённые пункты, оставляя только с кириллическими именами
    places <- places[grepl("[А-Яа-яЁё]", places$name), ]
    
    return(places)
  }
  
  # Если файл отсутствует — делает запросы к OSM для каждого региона из списка
  places_list <- lapply(region_names, function(name) {
    message("Запрашиваю: ", name)
    
    # Формирует запрос к OpenStreetMap по ключу place (город, село и т.п.)
    query <- opq(name) %>%
      add_osm_feature(key = "place", value = c("city", "town", "village", "hamlet"))
    
    # Выполняет запрос и ловит возможные ошибки
    result <- tryCatch(osmdata_sf(query), error = function(e) {
      warning("Ошибка запроса для региона: ", name)
      return(NULL)
    })
    
    # Если запрос успешен и есть точки с населёнными пунктами
    if (!is.null(result) && !is.null(result$osm_points)) {
      df <- result$osm_points
      
      # Добавляет столбец region_name, если есть addr:region, иначе NA
      if ("addr:region" %in% names(df)) {
        df$region_name <- df$`addr:region`
      } else {
        df$region_name <- NA_character_
      }
      
      # Выбирает нужные столбцы и преобразует в WGS84 (EPSG:4326)
      df <- df %>%
        dplyr::select(name, place, region_name, geometry, dplyr::everything()) %>%
        sf::st_transform(4326)
      
      # Фильтрует по кириллическим именам населённых пунктов
      df <- df[grepl("[А-Яа-яЁё]", df$name), ]
      
      return(df)
    }
    return(NULL)
  })
  
  # Удаляет пустые результаты запросов
  places_list <- places_list[!sapply(places_list, is.null)]
  
  # Если данные не получены, сообщает и возвращает NULL
  if (length(places_list) == 0) {
    message("Не удалось получить населённые пункты.")
    return(NULL)
  }
  
  # Объединяет все результаты в один sf-объект
  combined <- dplyr::bind_rows(places_list)
  
  # Создаёт папку для сохранения, если её нет
  dir.create(dirname(save_path), showWarnings = FALSE, recursive = TRUE)
  
  # Сохраняет объединённые населённые пункты в файл GeoPackage
  sf::st_write(combined, save_path, delete_dsn = TRUE, quiet = TRUE)
  message("Населённые пункты сохранены в: ", save_path)
  
  # Возвращает объединённый набор населённых пунктов
  return(combined)
}
