# Трансформация переменных
transform_data_nc <- function(data) {
  if (nrow(data) > 0) {
    data <- data %>%
      mutate(
        date_time = as.Date(as.POSIXct(valid_time, origin = "1970-01-01", tz = "UTC")),  # Преобразуем timestamp в дату
        total_precip_mm = as.numeric(tp),  # Осадки в м (ERA5 хранит в метрах)
        wind_speed_m = round(sqrt(as.numeric(u10)^2 + as.numeric(v10)^2), 1),  # Модуль ветра на 10 м
        temperature_air_C = as.numeric(t2m) - 273.15,  # Температура воздуха 2м в °C
        soil_temperature_level_1_C = as.numeric(stl1) - 273.15,  # Температура почвы 1 слоя (0-7 см)
        soil_temperature_level_2_C = as.numeric(stl2) - 273.15,  # Температура почвы 2 слоя (7-28 см)
        soil_temperature_level_3_C = as.numeric(stl3) - 273.15,  # Температура почвы 3 слоя (28-100 см)
        angle_of_direction_deg = (atan2(as.numeric(v10), as.numeric(u10)) * 180) / pi,  # Угол направления ветра в градусах
        surface_solar_radiation_downwards_J_m2 = round(as.numeric(ssrd), 2),  # Солнечная радиация, J/m^2
        leaf_index_high_vegetation_m_2_m_2 = round(as.numeric(lai_hv), 2),  # Листовой индекс высокой растительности
        leaf_index_low_vegetation_m_2_m_2 = round(as.numeric(lai_lv), 2)  # Листовой индекс низкой растительности
      ) %>% 
      select(x, y, date_time, total_precip_mm, wind_speed_m, temperature_air_C, 
             soil_temperature_level_1_C, soil_temperature_level_2_C, soil_temperature_level_3_C,
             surface_solar_radiation_downwards_J_m2, angle_of_direction_deg, leaf_index_high_vegetation_m_2_m_2, leaf_index_low_vegetation_m_2_m_2)
    
    message("Трансформация выполнена!")
    return(data)
  } else {
    message("Пустой датафрейм")
    return(NULL)
  }
}