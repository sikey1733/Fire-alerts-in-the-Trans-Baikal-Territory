# Фильтрация и отправка данных
filter_and_notify <- function(fire_sf,
                              weather_day_df,
                              bot_token = Sys.getenv("TELEGRAM_TOKEN"),
                              chat_id = Sys.getenv("TELEGRAM_CHAT_ID")) {
  
  if (bot_token == "" || chat_id == "") {
    message("❌ TELEGRAM_TOKEN или TELEGRAM_CHAT_ID не заданы.")
    return(NULL)
  }
  
  # 1. Фильтруем критические пожары, используя fire_sf (с расстояниями) и погодные данные
  fire_dist <- filter_critical_fires_dynamic(fire_sf, weather_day_df, region_names = NULL)  # Можно передавать region_names, если нужно
  
  if (is.null(fire_dist) || nrow(fire_dist) == 0) {
    message("Нет данных о критических пожарах для отправки уведомления.")
    return(NULL)
  }
  
  # 2. Находим минимальные расстояния для ближайшей точки (по населённому пункту и воде)
  fire_dist_min <- min(fire_dist$distance_to_settlement_km, na.rm = TRUE)
  
  # Ищем ближайший пожар по населённому пункту
  nearest_fire <- fire_dist %>%
    filter(!is.na(settlement_name)) %>%
    arrange(distance_to_settlement_km) %>%
    slice(1)
  
  # Берём расстояние до ближайшего водоёма именно для этой точки
  fire_dist_min_water <- nearest_fire$distance_to_water_km
  
  nearest_name <- nearest_fire$settlement_name
  fire_power <- nearest_fire$frp
  
  possible_region_cols <- c("settlement_region", "addr:region", "addr.region", "region_name")
  nearest_region <- NA_character_
  for (colname in possible_region_cols) {
    if (colname %in% names(nearest_fire)) {
      nearest_region <- as.character(nearest_fire[[colname]])
      break
    }
  }
  
  # 3. Вычисляем уровень риска пожара (по погоде)
  factor_data <- calc_fire_risk_flag(weather_day_df)
  if (is.null(factor_data)) factor_data <- "Неизвестен"
  
  # 4. Генерируем карту
  plot_nearest_fire_map(fire_dist, get_all_places(), get_all_waterbodies())
  
  # 5. Формируем сообщение
  msg <- paste0(
  "🔥 *Пожарный риск:*\n", 
  "➤ Уровень распространения огня: *", factor_data, "*\n\n",
  
  "📍 *Ближайшее возгорание:*\n", 
  "• Населённый пункт: *", nearest_name, 
  if (!is.na(nearest_region) && nearest_region != "") paste0(" (", nearest_region, ")") else "", "*\n",
  "• Расстояние до поселения: *", round(fire_dist_min, 2), " км*\n",
  "• Расстояние до воды: *", round(fire_dist_min_water, 2), " км*\n",
  "• Мощность излучения: *", fire_power, " МВт*"
  )
  
  # 6. Отправляем сообщение и изображение
  tryCatch({
    send_telegram_message(bot_token, chat_id, msg)
    message("✅ Сообщение успешно отправлено в Telegram.")
  }, error = function(e) {
    message("❌ Ошибка при отправке текстового сообщения: ", e$message)
  })
  
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
