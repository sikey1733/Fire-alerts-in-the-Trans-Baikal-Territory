# Функция отправляет изображение в Telegram через бот API по указанному пути к файлу
send_telegram_image <- function(bot_token = Sys.getenv("TELEGRAM_TOKEN"),
                                chat_id = Sys.getenv("TELEGRAM_CHAT_ID"), 
                                image_path = "output/nearest_fire_map_ggplot.png") {
  
  # Проверяет существование файла изображения
  if (!file.exists(image_path)) {
    warning("Файл для отправки не найден: ", image_path)
    return(FALSE)
  }
  
  # Формирует URL для запроса к Telegram Bot API для отправки фото
  url <- paste0("https://api.telegram.org/bot", bot_token, "/sendPhoto")
  
  # Выполняет POST-запрос с загрузкой файла методом multipart/form-data
  tryCatch({
    res <- httr::POST(
      url,
      body = list(
        chat_id = chat_id,
        photo = httr::upload_file(image_path)
      ),
      encode = "multipart"
    )
    
    # Проверяет статус ответа — 200 значит успешно
    if (status_code(res) == 200) {
      message("Изображение успешно отправлено в Telegram: ", basename(image_path))
      return(TRUE)
    } else {
      warning("Ошибка при отправке изображения в Telegram: ", httr::content(res, "text"))
      return(FALSE)
    }
  }, error = function(e) {
    # Обработка ошибок при отправке запроса
    warning("Ошибка при попытке отправить изображение: ", e$message)
    return(FALSE)
  })
}
