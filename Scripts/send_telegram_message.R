# Функция отправки сообщения в Telegram
send_telegram_message <- function(bot_token = Sys.getenv("TELEGRAM_TOKEN"),
                                  chat_id = Sys.getenv("TELEGRAM_CHAT_ID"), message_text) {
  url <- paste0("https://api.telegram.org/bot", bot_token, "/sendMessage")
  res <- POST(url, body = list(
    chat_id = chat_id,
    text = message_text,
    parse_mode = "Markdown"
  ), encode = "form")
  
  if (status_code(res) == 200) {
    message("Сообщение успешно отправлено в Telegram.")
    return(TRUE)
  } else {
    warning("Ошибка при отправке сообщения в Telegram: ", content(res, "text"))
    return(FALSE)
  }
}