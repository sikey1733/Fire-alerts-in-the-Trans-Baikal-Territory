# Функция-обертка на основе filter_critical_fires_dynamic для отправки уведомления
filter_and_notify <- function(weather_day_df,
                              bot_token = Sys.getenv("TELEGRAM_TOKEN"),
                              chat_id = Sys.getenv("TELEGRAM_CHAT_ID")) {

  if (bot_token == "" || chat_id == "") {
    message("❌ TELEGRAM_TOKEN или TELEGRAM_CHAT_ID не заданы.")
    return(NULL)
  }

  # 1. Фильтрация критических пожаров
  fire_dist <- filter_critical_fires_dynamic(weather_day_df)
  if (is.null(fire_dist) || nrow(fire_dist) == 0) {
    message("Нет данных о критических пожарах для отправки уведомления.")
    return(NULL)
  }

  # 2. Расчёт минимальных расстояний
  fire_dist_min <- min(fire_dist$distance_to_settlement_km, na.rm = TRUE)
  fire_dist_min_water <- min(fire_dist$distance_to_water_km, na.rm = TRUE)

  # 3. Ближайшая точка с названием и регионом
  nearest_fire <- fire_dist %>%
    filter(!is.na(settlement_name)) %>%
    arrange(distance_to_settlement_km) %>%
    slice(1)

  nearest_name <- nearest_fire$settlement_name

  # 4. Регион — безопасно ищем среди возможных имён колонок
  possible_region_cols <- c("settlement_region", "addr:region", "addr.region", "region_name")
  nearest_region <- NA_character_
  for (colname in possible_region_cols) {
    if (colname %in% names(nearest_fire)) {
      nearest_region <- as.character(nearest_fire[[colname]])
      break
    }
  }

  # 5. Повторно получаем уровень риска
  factor_data <- calc_fire_risk_flag(weather_day_df)
  if (is.null(factor_data)) factor_data <- "Неизвестен"

  # 6. Генерация карты
  plot_nearest_fire_map(fire_dist, get_all_places(), get_all_waterbodies())

  # 7. Формируем сообщение
  msg <- paste0(
    "🔥 *Уровень риска распространения огня:* ", factor_data, "\n",
    "📍 *Минимальное расстояние до населённого пункта:* ", round(fire_dist_min, 2), " км\n",
    "🏘️ *Ближайший населённый пункт:* ", nearest_name,
    if (!is.na(nearest_region) && nearest_region != "") paste0(" (", nearest_region, ")") else "", "\n",
    "💧 *Ближайший водоём:* ", round(fire_dist_min_water, 2), " км"
  )

  # 8. Отправка текста
  tryCatch({
    send_telegram_message(bot_token, chat_id, msg)
    message("✅ Сообщение успешно отправлено в Telegram.")
  }, error = function(e) {
    message("❌ Ошибка при отправке текстового сообщения: ", e$message)
  })

  # 9. Отправка карты
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
