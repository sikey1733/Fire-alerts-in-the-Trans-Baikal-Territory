# Очистка 
clear_na_nc <- function(clear_data) {
  
  # Проверка: есть ли вообще пропущенные значения
  if (anyNA(clear_data)) {
    message("Найдены пропущенные значения!")
    
    # Показать количество NA по столбцам
    print(colSums(is.na(clear_data)))
    
    message("Начинаю удаление строк с пропущенными значениями...")
    
    # Удаление строк с NA
    clean_data <- drop_na(clear_data)
    
    message("Пропущенные значения удалены!")
    return(clean_data)
    
  } else {
    message("Пропущенных значений не найдено!")
    return(clear_data)
  }
}