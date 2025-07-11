# Функция читает CSV с данными VIIRS из папки
read_data_viirs <- function(data_dir = "data/") {
  # Получает список всех CSV-файлов в папке data_dir
  csv_files <- list.files(path = data_dir, pattern = "\\.csv$", full.names = TRUE)
  
  # Проверяет, есть ли CSV-файл; если нет — выводит сообщение и возвращает NULL
  if (length(csv_files) == 0) {
    message("Нет CSV-файлов в папке: ", data_dir)
    return(NULL)
  }
  
  # Сортирует файлы по дате изменения (сначала самый новый)
  csv_files <- csv_files[order(file.info(csv_files)$mtime, decreasing = TRUE)]
  
  # Выбирает самый свежий файл
  file_path <- csv_files[1]
  message("Файл найден: ", basename(file_path))
  
  # Пытается прочитать CSV с кодировкой UTF-8, при ошибке — сообщает и возвращает NULL
  data <- tryCatch({
    read.csv(file_path, stringsAsFactors = FALSE, fileEncoding = "UTF-8")
  }, error = function(e) {
    message("Ошибка чтения CSV: ", e$message)
    return(NULL)
  })
  
  # Проверяет наличие обязательных колонок longitude, latitude, confidence
  required_cols <- c("longitude", "latitude", "confidence")
  missing_cols <- setdiff(required_cols, names(data))
  
  # Если отсутствует какая-либо колонка — выводит сообщение и возвращает NULL
  if (length(missing_cols) > 0) {
    message("В файле отсутствует колонка(и): ", paste(missing_cols, collapse = ", "))
    return(NULL)
  }
  
  # Возвращает считанные данные
  return(data)
}
