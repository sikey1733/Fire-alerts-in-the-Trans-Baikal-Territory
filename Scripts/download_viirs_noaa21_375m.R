# Функция запроса данных NASA FIRMS за последний день
download_viirs_noaa21_375m <- function(map_key = Sys.getenv("VIIRS_KEY")) {
  
  base_url <- "https://firms.modaps.eosdis.nasa.gov/api/area/csv"
  product <- "VIIRS_NOAA21_NRT"
  bbox <- paste(c(102.7, 46.7, 130.7, 63.6), collapse = ",")
  days <- "1"
  url <- sprintf("%s/%s/%s/%s/%s", base_url, map_key, product, bbox, days)
  data_dir <- "data"

  if (!dir.exists(data_dir)) dir.create(data_dir, recursive = TRUE)
  
  destfile <- file.path(data_dir, "viirs_noaa21_375m.csv")
  
  tryCatch({
    download.file(url, destfile, mode = "wb", quiet = TRUE)
    message("Данные успешно загружены в: ", destfile)
    return(destfile)
  }, error = function(e) {
    message("Ошибка загрузки данных: ", e$message)
    return(NULL)
  })
}