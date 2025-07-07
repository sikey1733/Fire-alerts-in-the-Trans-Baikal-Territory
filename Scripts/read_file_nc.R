# Чтение, извлечение, обьединение
read_file_nc <- function(data_dir = "data/") {
  
  if (!dir.exists(data_dir)) {
    message("Папка ", data_dir, " не существует.")
    return(NULL)
  }
  
  file_list <- list.files(data_dir, pattern = "\\.nc$", full.names = TRUE)
  
  if (length(file_list) == 0) {
    message("Данные не поступали!")
    return(NULL)
  } else if (length(file_list) < 2) {
    message("Недостаточно файлов для объединения (нужно минимум 2).")
    return(NULL)
  }
  
  file_list <- file_list[order(file.info(file_list)$mtime, decreasing = TRUE)]
  message("Использую два последних файла: ", basename(file_list[1]), " и ", basename(file_list[2]))
  
  file_1 <- read_stars(file_list[1]) %>% as.data.frame(xy = TRUE)
  file_2 <- read_stars(file_list[2]) %>% as.data.frame(xy = TRUE)
  coords_match <- all(file_1$x == file_2$x) && all(file_1$y == file_2$y)
  
  if (!coords_match) {
    message("Координаты не совпадают! Проверьте исходные .nc файлы.")
    return(NULL)
  }
  
  common_cols <- intersect(names(file_1), names(file_2))
  key_cols <- intersect(c("x", "y", "valid_time"), common_cols)
  
  if (length(key_cols) < 2) {
    message("Недостаточно общих колонок для объединения.")
    return(NULL)
  }
  
  data_join <- full_join(file_1, file_2, by = key_cols)
  
  if (nrow(data_join) > 0) {
    message("Данные успешно объединены! Кол-во строк: ", nrow(data_join))
    return(data_join)
  } else {
    message("Объединение дало пустой результат.")
    return(NULL)
  }
}