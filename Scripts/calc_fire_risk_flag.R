# Функция вычисляет флаг риска пожара на основе погодных и почвенных данных за один день
calc_fire_risk_flag <- function(day_df) {
  # Определяет обязательные колонки для анализа
  required_cols <- c("temperature_air_C", "total_precip_mm", "wind_speed_m", 
                     "soil_temperature_level_1_C", "soil_temperature_level_2_C",
                     "leaf_index_low_vegetation_m_2_m_2")
  
  # Проверяет, что данные существуют и не пусты
  if (is.null(day_df) || nrow(day_df) == 0) {
    warning("Данные за день отсутствуют или пусты")
    return(NA)
  }
  
  # Проверяет наличие всех обязательных колонок
  if (!all(required_cols %in% names(day_df))) {
    warning("Отсутствуют нужные колонки в данных: ", 
            paste(setdiff(required_cols, names(day_df)), collapse = ", "))
    return(NA)
  }
  
  # Рассчитывает средние значения температуры воздуха, ветра, температуры почвы и индекс листвы
  mean_temp <- mean(day_df$temperature_air_C, na.rm = TRUE)
  sum_precip <- max(day_df$total_precip_mm, na.rm = TRUE)
  if (is.na(sum_precip)) sum_precip <- 0  # Если осадков нет — считаем 0
  mean_wind <- mean(day_df$wind_speed_m, na.rm = TRUE)
  stl1 <- mean(day_df$soil_temperature_level_1_C, na.rm = TRUE)
  stl2 <- mean(day_df$soil_temperature_level_2_C, na.rm = TRUE)
  leaf_index <- mean(day_df$leaf_index_low_vegetation_m_2_m_2, na.rm = TRUE)
  
  # Логика определения уровня риска пожара
  if (mean_temp >= 15 && sum_precip < 10 && mean_wind >= 3 && stl1 >= 5 && stl2 >= 5) {
    return("Высокий")
  } else if (mean_temp >= 10 && sum_precip < 15 && mean_wind >= 2) {
    return("Средний")
  } else if (mean_temp < 5 && sum_precip > 50 && mean_wind < 2 && stl1 < 2 && stl2 < 2) {
    return("Низкий")
  } else {
    return("Нет риска")
  }
}
