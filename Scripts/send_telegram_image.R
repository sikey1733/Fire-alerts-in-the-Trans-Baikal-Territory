# Функция отправки изображения в Telegram с использованием последнего файла
send_telegram_image <- function(bot_token = Sys.getenv("TELEGRAM_TOKEN"),
                                chat_id = Sys.getenv("TELEGRAM_CHAT_ID"), 
                                directory = "output/") {
  
  image_path <- get_latest_file(directory)
  
  if (is.null(image_path)) {
    warning("Нет изображений для отправки.")
    return(FALSE)
  }
  
  url <- paste0("https://api.telegram.org/bot", bot_token, "/sendPhoto")
  res <- POST(url, 
              body = list(
                chat_id = chat_id,
                photo = upload_file(image_path)
              ), 
              encode = "multipart")
  
  if (status_code(res) == 200) {
    message("Изображение успешно отправлено в Telegram.")
    return(TRUE)
  } else {
    warning("Ошибка при отправке изображения в Telegram: ", content(res, "text"))
    return(FALSE)
  }
}