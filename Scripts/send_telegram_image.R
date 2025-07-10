# Функция отправки изображения в Telegram с использованием последнего файла
send_telegram_image <- function(bot_token = Sys.getenv("TELEGRAM_TOKEN"),
                                chat_id = Sys.getenv("TELEGRAM_CHAT_ID"), 
                                image_path = "output/nearest_fire_map_ggplot.png") {
  
  if (!file.exists(image_path)) {
    warning("Файл для отправки не найден: ", image_path)
    return(FALSE)
  }
  
  url <- paste0("https://api.telegram.org/bot", bot_token, "/sendPhoto")
  
  tryCatch({
    res <- httr::POST(
      url,
      body = list(
        chat_id = chat_id,
        photo = httr::upload_file(image_path)
      ),
      encode = "multipart"
    )
    
    if (httr::status_code(res) == 200) {
      message("✅ Изображение успешно отправлено в Telegram: ", basename(image_path))
      return(TRUE)
    } else {
      warning("❌ Ошибка при отправке изображения в Telegram: ", httr::content(res, "text"))
      return(FALSE)
    }
  }, error = function(e) {
    warning("❌ Ошибка при попытке отправить изображение: ", e$message)
    return(FALSE)
  })
}
