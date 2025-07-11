# Функция определяет дистанцию для риска распространения огня на основе данных за день
filter_critical_fires_dynamic <- function(fire_sf, weather_day_df, region_names = NULL) {
  if (is.null(fire_sf) || nrow(fire_sf) == 0) {
    message("❌ Нет данных о пожарах для фильтрации.")
    return(NULL)
  }
  if (is.null(weather_day_df) || nrow(weather_day_df) == 0) {
    message("❌ Нет погодных данных для фильтрации.")
    return(NULL)
  }
  
  # Проверяем, есть ли расстояния, если нет — рассчитываем
  if (!("distance_to_settlement_km" %in% colnames(fire_sf)) ||
      !("distance_to_water_km" %in% colnames(fire_sf))) {
    fire_sf <- calculate_fire_distances(fire_sf, region_names)
    if (is.null(fire_sf)) return(NULL)
  }
  
  # Рассчитываем флаг риска по погодным данным
  risk_flag <- calc_fire_risk_flag(weather_day_df)
  
  # Фильтруем пожары по расстоянию (примерно)
  critical_fires <- fire_sf %>%
    dplyr::filter(!is.na(distance_to_settlement_km)) %>%
    dplyr::filter(distance_to_settlement_km < 10)  # Например, ближе 10 км
  
  if (nrow(critical_fires) == 0) {
    message("❌ Нет критических пожаров по расстоянию.")
    return(NULL)
  }
  
  # Если риск не высокий — возвращаем NULL
  if (risk_flag != "Высокий риск") {
    message("⚠️ Риск пожара низкий или средний - критических пожаров нет.")
    return(NULL)
  }
  
  critical_fires$risk_flag <- risk_flag
  return(critical_fires)
}
