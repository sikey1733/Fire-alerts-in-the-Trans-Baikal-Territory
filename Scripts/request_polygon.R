# Запрос полигонов OSM
request_polygon <- function(region_names, save_path = "data/polygons.gpkg") {
  
  if (file.exists(save_path)) {
    message("Загружаю полигоны из файла: ", save_path)
    polygons <- tryCatch(st_read(save_path, quiet = TRUE),
                         error = function(e) {
                           message("Ошибка чтения файла полигонов: ", e$message)
                           return(NULL)
                         })
    return(polygons)
  }
  
  polygons <- lapply(region_names, function(name) {
    message("Запрашиваю: ", name)
    bb <- tryCatch(
      getbb(name, format_out = "sf_polygon"),
      error = function(e) {
        warning("Ошибка запроса региона: ", name, " - ", e$message)
        return(NULL)
      }
    )
    # Проверка, что bb - sf POLYGON или MULTIPOLYGON
    if (!is.null(bb) && inherits(bb, "sf")) {
      bb$region_name <- name
      return(bb)
    } else {
      warning("Регион ", name, " не вернул полигон.")
      return(NULL)
    }
  })
  
  polygons <- polygons[!sapply(polygons, is.null)]
  
  if (length(polygons) == 0) {
    message("Не удалось получить полигоны для всех регионов.")
    return(NULL)
  }
  
  combined <- tryCatch(
    do.call(rbind, polygons),
    error = function(e) {
      message("Ошибка объединения полигонов: ", e$message)
      return(NULL)
    }
  )
  
  if (is.null(combined)) return(NULL)
  
  dir.create(dirname(save_path), showWarnings = FALSE, recursive = TRUE)
  
  tryCatch(
    st_write(combined, save_path, delete_dsn = TRUE, quiet = TRUE),
    error = function(e) {
      message("Ошибка сохранения полигонов: ", e$message)
    }
  )
  
  message("Полигоны сохранены в: ", save_path)
  
  return(combined)
}
