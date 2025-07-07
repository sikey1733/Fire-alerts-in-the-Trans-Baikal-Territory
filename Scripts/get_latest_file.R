# Функция для поиска последнего измененного файла в папке
get_latest_file <- function(directory = "output/") {
  
  files <- list.files(directory, full.names = TRUE)
  
  if (length(files) == 0) {
    warning("Нет файлов в указанной папке: ", directory)
    return(NULL)
  }
  
  latest_file <- files[which.max(file.info(files)$mtime)]
  message("Последний файл: ", latest_file)
  
  return(latest_file)
}