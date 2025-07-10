# Получение водоёмов через OSM
get_all_waterbodies <- function(region_names, save_path = "data/waterbodies.gpkg") {
  if (file.exists(save_path)) {
    message("Загружаю водоёмы из файла: ", save_path)
    waterbodies <- st_read(save_path, quiet = TRUE)
    
    # Фильтрация по кириллице в названии (если есть колонка name)
    if ("name" %in% names(waterbodies)) {
      waterbodies <- waterbodies[grepl("[А-Яа-яЁё]", waterbodies$name), ]
    }
    
    return(waterbodies)
  }
  
  water_list <- lapply(region_names, function(name) {
    message("Запрашиваю водоёмы: ", name)
    query <- opq(name) %>%
      add_osm_feature(key = "natural", value = "water")
    
    result <- tryCatch(osmdata_sf(query), error = function(e) return(NULL))
    
    if (!is.null(result) && !is.null(result$osm_polygons)) {
      df <- result$osm_polygons %>%
        select(name, geometry) %>%
        st_transform(4326)
      
      # Фильтрация по кириллице
      if ("name" %in% names(df)) {
        df <- df[grepl("[А-Яа-яЁё]", df$name), ]
      }
      
      return(df)
    }
    return(NULL)
  })
  
  water_list <- water_list[!sapply(water_list, is.null)]
  if (length(water_list) == 0) {
    message("Не удалось получить водоёмы.")
    return(NULL)
  }
  
  combined <- bind_rows(water_list)
  
  dir.create(dirname(save_path), showWarnings = FALSE, recursive = TRUE)
  st_write(combined, save_path, delete_dsn = TRUE, quiet = TRUE)
  message("Водоёмы сохранены в: ", save_path)
  
  return(combined)
}
