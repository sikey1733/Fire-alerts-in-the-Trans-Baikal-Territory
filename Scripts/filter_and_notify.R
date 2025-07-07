# Функция-обертка на основе filter_critical_fires_dynamic для отправки уведомления
filter_and_notify <- function(weather_day_df, bot_token = Sys.getenv("TELEGRAM_TOKEN"),
                              chat_id = Sys.getenv("TELEGRAM_CHAT_ID"), 
                              image_directory = "output/") {
  
  # 1. Фильтрация критических пожаров
  fire_dist <- filter_critical_fires_dynamic(weather_day_df)
  if (is.null(fire_dist) || nrow(fire_dist) == 0) {
    message("Нет данных о критических пожарах для отправки уведомления.")
    return(NULL)
  }
  
  # 2. Расчёт уровня риска
  factor_data <- calc_fire_risk_flag(weather_day_df)
  if (is.null(factor_data)) {
    message("Не удалось рассчитать уровень риска.")
    return(NULL)
  }
  
  # 3. Расчёт минимальных расстояний
  fire_dist_min <- min(fire_dist$distance_to_settlement_km, na.rm = TRUE)
  fire_dist_min_water <- min(fire_dist$distance_to_water_km, na.rm = TRUE)
  
  # 4. Получение ближайшего населённого пункта
  nearest_name <- fire_dist %>%
    filter(!is.na(settlement_name)) %>%
    arrange(distance_to_settlement_km) %>%
    slice(1) %>%
    pull(settlement_name)
  
  # 5. Составление сообщения
  msg <- paste0(
    "🔥 *Уровень риска распространения огня:* ", factor_data, "\n",
    "📍 *Минимальное расстояние до населённого пункта:* ", round(fire_dist_min, 2), " км\n",
    "🏘️ *Ближайший населённый пункт:* ", nearest_name, "\n",
    "💧 *Ближайший водоём:* ", round(fire_dist_min_water, 2), " км"
  )
  
  # 6. Отправка сообщения в Telegram
  tryCatch({
    send_telegram_message(bot_token, chat_id, msg)
    message("Сообщение успешно отправлено в Telegram.")
  }, error = function(e) {
    message("Ошибка при отправке сообщения в Telegram: ", e$message)
  })
  
  # 7. Отправка изображения
  tryCatch({
    send_telegram_image(bot_token, chat_id, image_directory)
    message("Изображение успешно отправлено в Telegram.")
  }, error = function(e) {
    message("Ошибка при отправке изображения в Telegram: ", e$message)
  })
}
