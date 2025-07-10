get_all_places <- function(region_names, save_path = "data/places.gpkg") {
  if (file.exists(save_path)) {
    message("Загружаю населённые пункты из файла: ", save_path)
    places <- sf::st_read(save_path, quiet = TRUE)
    
    # Фильтрация по кириллице
    places <- places[grepl("[А-Яа-яЁё]", places$name), ]
    
    return(places)
  }
  
  places_list <- lapply(region_names, function(name) {
    message("Запрашиваю: ", name)
    query <- opq(name) %>%
      add_osm_feature(key = "place", value = c("city", "town", "village", "hamlet"))
    
    result <- tryCatch(osmdata_sf(query), error = function(e) {
      warning("Ошибка запроса для региона: ", name)
      return(NULL)
    })
    
    if (!is.null(result) && !is.null(result$osm_points)) {
      df <- result$osm_points %>%
        # Добавляем поле addr:region, если есть (в R это addr.region)
        dplyr::select(name, place, geometry, dplyr::everything()) %>%
        sf::st_transform(4326)
      
      # Фильтрация по кириллице
      df <- df[grepl("[А-Яа-яЁё]", df$name), ]
      
      return(df)
    }
    return(NULL)
  })
  
  places_list <- places_list[!sapply(places_list, is.null)]
  if (length(places_list) == 0) {
    message("Не удалось получить населённые пункты.")
    return(NULL)
  }
  
  combined <- dplyr::bind_rows(places_list)
  
  dir.create(dirname(save_path), showWarnings = FALSE, recursive = TRUE)
  sf::st_write(combined, save_path, delete_dsn = TRUE, quiet = TRUE)
  message("Населённые пункты сохранены в: ", save_path)
  
  return(combined)
}
