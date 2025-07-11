# Функция для вычисления флага риска по данным за 1 день
calc_fire_risk_flag <- function(day_df) {
  required_cols <- c("temperature_air_C", "total_precip_mm", "wind_speed_m", 
                     "soil_temperature_level_1_C", "soil_temperature_level_2_C",
                    "leaf_index_low_vegetation_m_2_m_2")
  
  if (is.null(day_df) || nrow(day_df) == 0) {
    warning("Данные за день отсутствуют или пусты")
    return(NA)
  }
  
  if (!all(required_cols %in% names(day_df))) {
    warning("Отсутствуют нужные колонки в данных: ", 
            paste(setdiff(required_cols, names(day_df)), collapse = ", "))
    return(NA)
  }
  
  mean_temp <- mean(day_df$temperature_air_C, na.rm = TRUE)
  sum_precip <- max(day_df$total_precip_mm, na.rm = TRUE)
  if (is.na(sum_precip)) sum_precip <- 0
  mean_wind <- mean(day_df$wind_speed_m, na.rm = TRUE)
  stl1 <- mean(day_df$soil_temperature_level_1_C, na.rm = TRUE)
  stl2 <- mean(day_df$soil_temperature_level_2_C, na.rm = TRUE)
  leaf_index <- mean(day_df$leaf_index_low_vegetation_m_2_m_2, na.rm = TRUE)
  
  if (mean_temp >= 15 && sum_precip < 10 && mean_wind >= 3 && stl1 >= 5 && stl2 >= 5) {
    return("Высокий риск")
  } else if (mean_temp >= 10 && sum_precip < 15 && mean_wind >= 2) {
    return("Средний риск")
  } else if (mean_temp < 5 && sum_precip > 50 && mean_wind < 2 && stl1 < 2 && stl2 < 2) {
    return("Низкий риск")
  } else {
    return("Нет риска")
  }
}
