# Функция определения дистанции по уровню риска распространения огня
filter_critical_fires_dynamic <- function(weather_day_df) {
  
  factor_data <- calc_fire_risk_flag(weather_day_df)
  fire_dist <- calculate_fire_distances()
  
  if (is.null(factor_data) || is.null(fire_dist)) {
    message("Нет данных по погоде или пожарам!")
    return(NULL)
  }

  fire_dist_min <- min(fire_dist$distance_to_settlement_km, na.rm = TRUE)
  fire_dist_min_water <- min(fire_dist$distance_to_water_km, na.rm = TRUE)

  nearest_fire <- fire_dist %>%
    filter(!is.na(settlement_name)) %>%
    arrange(distance_to_settlement_km) %>%
    slice(1)

  nearest_name <- nearest_fire$settlement_name
  nearest_region <- if ("settlement_region" %in% colnames(nearest_fire)) {
    nearest_fire$settlement_region
  } else {
    NA_character_
  }

  message("🔥 Уровень риска распространения огня: ", factor_data)
  message("📍 Минимальное расстояние до населённого пункта: ", round(fire_dist_min, 2), " км")
  message("🏘️ Ближайший населённый пункт: ", nearest_name,
          if (!is.na(nearest_region)) paste0(" (", nearest_region, ")") else "")
  message("💧 Ближайший водоём: ", round(fire_dist_min_water, 2), " км")

  return(fire_dist)
}
