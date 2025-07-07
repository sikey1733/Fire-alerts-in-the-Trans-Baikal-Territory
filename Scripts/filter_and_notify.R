# Функция-обертка на основе filter_critical_fires_dynamic для отправки уведомления
filter_and_notify <- function(weather_day_df, bot_token = Sys.getenv("TELEGRAM_TOKEN"),
                              chat_id = Sys.getenv("TELEGRAM_CHAT_ID"), 
                              image_directory = "output/") {
  fire_dist <- filter_critical_fires_dynamic(weather_day_df)
  if (is.null(fire_dist)) {
    message("Нет данных для отправки уведомления.")
    return(NULL)
  }
  
  factor_data <- calc_fire_risk_flag(weather_day_df)
  fire_dist_min <- min(fire_dist$distance_to_settlement_km, na.rm = TRUE)
  fire_dist_min_water <- min(fire_dist$distance_to_water_km, na.rm = TRUE)
  
  nearest_name <- fire_dist %>%
    filter(!is.na(settlement_name)) %>%
    arrange(distance_to_settlement_km) %>%
    slice(1) %>%
    pull(settlement_name)
  
  msg <- paste0(
    "🔥 *Уровень риска распространения огня:* ", factor_data, "\n",
    "📍 *Минимальное расстояние до населённого пункта:* ", round(fire_dist_min, 2), " км\n",
    "🏘️ *Ближайший населённый пункт:* ", nearest_name, "\n",
    "💧 *Ближайший водоём:* ", round(fire_dist_min_water, 2), " км"
  )
  
  send_telegram_message(bot_token, chat_id, msg)
  send_telegram_image(bot_token, chat_id, image_directory)
}