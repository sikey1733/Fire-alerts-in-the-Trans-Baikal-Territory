filter_and_notify <- function(weather_day_df,
                              bot_token = Sys.getenv("TELEGRAM_TOKEN"),
                              chat_id = Sys.getenv("TELEGRAM_CHAT_ID")) {

  # Проверяет наличие токена и ID чата Telegram
  if (bot_token == "" || chat_id == "") {
    message("❌ TELEGRAM_TOKEN или TELEGRAM_CHAT_ID не заданы.")
    return(NULL)
  }

  # 1. Фильтрует критические пожары по погодным данным (возвращает sf с расстояниями)
  fire_dist <- filter_critical_fires_dynamic(weather_day_df)
  if (is.null(fire_dist) || nrow(fire_dist) == 0) {
    message("Нет данных о критических пожарах для отправки уведомления.")
    return(NULL)
  }

  # 2. Находит ближайший пожар с известным населённым пунктом (минимальное расстояние до поселения)
  nearest_fire <- fire_dist %>%
    filter(!is.na(settlement_name)) %>%
    arrange(distance_to_settlement_km) %>%
    slice(1)

  # Получаем нужные данные для сообщения из ближайшего пожара
  nearest_name <- nearest_fire$settlement_name
  fire_dist_min <- nearest_fire$distance_to_settlement_km
  fire_dist_min_water <- nearest_fire$distance_to_water_km

  # 3. Безопасно ищет имя региона среди нескольких вариантов названий колонок
  possible_region_cols <- c("settlement_region", "addr:region", "addr.region", "region_name")
  nearest_region <- NA_character_
  for (colname in possible_region_cols) {
    if (colname %in% names(nearest_fire)) {
      nearest_region <- as.character(nearest_fire[[colname]])
      break
    }
  }

  # 4. Повторно вычисляет уровень риска пожара
  factor_data <- calc_fire_risk_flag(weather_day_df)
  if (is.null(factor_data)) factor_data <- "Неизвестен"

  # 5. Генерирует карту ближайшего пожара, населённого пункта и водоёма
  plot_nearest_fire_map(fire_dist, get_all_places(), get_all_waterbodies())

  # 6. Формирует текст сообщения с данными о риске и ближайших объектах
  msg <- paste0(
    "🔥 *Уровень риска распространения огня:* ", factor_data, "\n",
    "📍 *Минимальное расстояние до населённого пункта:* ", round(fire_dist_min, 2), " км\n",
    "🏘️ *Ближайший населённый пункт:* ", nearest_name,
    if (!is.na(nearest_region) && nearest_region != "") paste0(" (", nearest_region, ")") else "", "\n",
    "💧 *Ближайший водоём:* ", round(fire_dist_min_water, 2), " км"
  )

  # 7. Отправляет текстовое сообщение в Telegram
  tryCatch({
    send_telegram_message(bot_token, chat_id, msg)
    message("✅ Сообщение успешно отправлено в Telegram.")
  }, error = function(e) {
    message("❌ Ошибка при отправке текстового сообщения: ", e$message)
  })

  # 8. Отправляет карту изображением в Telegram, если файл существует
  map_path <- "output/nearest_fire_map_ggplot.png"
  if (file.exists(map_path)) {
    tryCatch({
      send_telegram_image(bot_token, chat_id, image_path = map_path)
      message("✅ Изображение карты отправлено в Telegram.")
    }, error = function(e) {
      message("⚠️ Ошибка при отправке изображения: ", e$message)
    })
  } else {
    message("⚠️ Картинка не найдена: ", map_path)
  }
}
