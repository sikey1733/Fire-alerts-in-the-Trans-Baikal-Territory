# Получение водоёмов через OSM
get_all_waterbodies <- function(region_names, save_path = "data/waterbodies.gpkg") {
  if (file.exists(save_path)) {
    message("Загружаю водоёмы из файла: ", save_path)
    waterbodies <- st_read(save_path, quiet = TRUE)
    return(waterbodies)
  }
  
  water_list <- lapply(region_names, function(name) {
    message("Запрашиваю водоёмы: ", name)
    query <- opq(name) %>%
      add_osm_feature(key = "natural", value = "water")
    
    result <- tryCatch(osmdata_sf(query), error = function(e) return(NULL))
    
    if (!is.null(result) && !is.null(result$osm_polygons)) {
      return(result$osm_polygons %>%
               select(geometry) %>%
               st_transform(4326))
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