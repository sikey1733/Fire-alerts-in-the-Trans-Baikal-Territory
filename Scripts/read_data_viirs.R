# Функция чтения "CSV"
read_data_viirs <- function(data_dir = "data/") {
  csv_files <- list.files(path = data_dir, pattern = "\\.csv$", full.names = TRUE)
  
  if (length(csv_files) == 0) {
    message("Нет CSV-файлов в папке: ", data_dir)
    return(NULL)
  }
  
  csv_files <- csv_files[order(file.info(csv_files)$mtime, decreasing = TRUE)]
  file_path <- csv_files[1]
  
  message("Файл найден: ", basename(file_path))
  
  data <- tryCatch({
    read.csv(file_path, stringsAsFactors = FALSE, fileEncoding = "UTF-8")
  }, error = function(e) {
    message("Ошибка чтения CSV: ", e$message)
    return(NULL)
  })
  
  required_cols <- c("longitude", "latitude", "confidence")
  missing_cols <- setdiff(required_cols, names(data))
  if (length(missing_cols) > 0) {
    message("В файле отсутствуют необходимые колонки: ", paste(missing_cols, collapse = ", "))
    return(NULL)
  }
  
  return(data)
}
