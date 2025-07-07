# Построение графика "Розы ветров"
roza_air_vizualization <- function(roza, output_file = "output/wind_rose.png") {
  
  required_vars <- c("wind_speed_m", "angle_of_direction_deg")
  if (!all(required_vars %in% names(roza))) {
    message("Отсутствуют нужные переменные: 'wind_speed_m' и 'angle_of_direction_deg'")
    return(NULL)
  }
  roza_clean <- roza %>%
    filter(
      !is.na(wind_speed_m), wind_speed_m > 0, wind_speed_m < 100,  
      !is.na(angle_of_direction_deg), angle_of_direction_deg >= -180, angle_of_direction_deg <= 180 
    )
  if (nrow(roza_clean) == 0) {
    message("Нет подходящих данных для построения розы ветров!")
    return(NULL)
  }
  message("Строю график розы ветров!")
  dir.create(dirname(output_file), showWarnings = FALSE, recursive = TRUE)
  
  png(filename = output_file, width = 800, height = 600)
  windRose(
    roza_clean,
    ws = "wind_speed_m", 
    wd = "angle_of_direction_deg",  
    breaks = c(0, 3, 6, 9, 12, 15, 20), 
    angle = 30,  
    cols = "jet",  
    paddle = FALSE, 
    key.footer = "Скорость ветра (м/с)", 
    key.position = "right"  
  )
  dev.off()  
  
  message("График сохранён в: ", output_file)
  
  return(output_file)
}
