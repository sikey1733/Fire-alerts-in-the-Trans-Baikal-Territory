# Функция определяет дистанцию для риска распространения огня на основе данных за день
filter_critical_fires_dynamic <- function(weather_day_df) {
  
  # Вычисляет уровень риска пожара по погодным данным (функция calc_fire_risk_flag)
  factor_data <- calc_fire_risk_flag(weather_day_df)
  
  # Вычисляет расстояния до населённых пунктов и водоёмов (функция calculate_fire_distances)
  fire_dist <- calculate_fire_distances()
  
  # Проверяет наличие необходимых данных
  if (is.null(factor_data) || is.null(fire_dist)) {
    message("Нет данных по погоде или пожарам!")
    return(NULL)
  }
  
  # Определяет минимальное расстояние до населённого пункта и водоёма
  fire_dist_min <- min(fire_dist$distance_to_settlement_km, na.rm = TRUE)
  fire_dist_min_water <- min(fire_dist$distance_to_water_km, na.rm = TRUE)
  
  # Находит ближайший пожар с известным названием населённого пункта
  nearest_fire <- fire_dist %>%
    filter(!is.na(settlement_name)) %>%
    arrange(distance_to_settlement_km) %>%
    slice(1)
  
  # Извлекает название и регион ближайшего населённого пункта
  nearest_name <- nearest_fire$settlement_name
  nearest_region <- if ("settlement_region" %in% colnames(nearest_fire)) {
    nearest_fire$settlement_region
  } else {
    NA_character_
  }
  
  # Выводит информацию о риске и ближайших объектах
  message("🔥 Уровень риска распространения огня: ", factor_data)
  message("📍 Минимальное расстояние до населённого пункта: ", round(fire_dist_min, 2), " км")
  message("🏘️ Ближайший населённый пункт: ", nearest_name,
          if (!is.na(nearest_region)) paste0(" (", nearest_region, ")") else "")
  message("💧 Ближайший водоём: ", round(fire_dist_min_water, 2), " км")
  
  # Возвращает объект с расстояниями и дополнительной информацией
  return(fire_dist)
}
