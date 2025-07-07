# Запрос полигонов OSM
request_polygon_OSM <- function(region_names, save_path = "data/polygons.gpkg") {
  
  if (file.exists(save_path)) {
    message("Загружаю полигоны из файла: ", save_path)
    polygons <- st_read(save_path, quiet = TRUE)
    return(polygons)
  }
  
  polygons <- lapply(region_names, function(name) {
    message("Запрашиваю: ", name)
    bb <- tryCatch(
      getbb(name, format_out = "sf_polygon"),
      error = function(e) {
        warning("Ошибка запроса региона: ", name)
        return(NULL)
      }
    )
    if (!is.null(bb)) {
      bb$region_name <- name
    }
    return(bb)
  })
  
  polygons <- polygons[!sapply(polygons, is.null)]
  if (length(polygons) == 0) {
    message("Не удалось получить полигоны.")
    return(NULL)
  }
  
  combined <- do.call(rbind, polygons)
  dir.create(dirname(save_path), showWarnings = FALSE, recursive = TRUE)
  st_write(combined, save_path, delete_dsn = TRUE, quiet = TRUE)
  message("Полигоны сохранены в: ", save_path)
  
  return(combined)
}