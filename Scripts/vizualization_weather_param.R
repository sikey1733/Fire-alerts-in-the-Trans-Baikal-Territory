# Построение графиков погодных параметров
vizualization_weather_param <- function(vizual) {
  if (!is.null(vizual) && nrow(vizual) > 0) {
    message("Агрегирую данные по дню и строю график погодных параметров!")
    
    data_agg <- vizual %>%
      group_by(date_time) %>%
      summarise(across(c(temperature_air_C, wind_speed_m, total_precip_mm,
                         soil_temperature_level_1_C, soil_temperature_level_2_C),
                       ~ mean(.x, na.rm = TRUE))) %>%
      pivot_longer(cols = -date_time, names_to = "variable", values_to = "value")
    
    max_points <- data_agg %>%
      group_by(variable) %>%
      filter(value == max(value, na.rm = TRUE)) %>%
      ungroup()
    
    p <- ggplot(data_agg, aes(x = date_time, y = value)) +
      geom_line(color = "steelblue") +
      geom_point(data = max_points, aes(x = date_time, y = value), color = "red", size = 2) +
      geom_text(
        data = max_points,
        aes(x = date_time, y = value, label = round(value, 1)),
        vjust = -0.8,
        color = "red",
        size = 3
      ) +
      facet_grid(rows = vars(variable), scales = "free_y") +
      scale_y_continuous(expand = expansion(mult = c(0.05, 0.2))) +
      scale_x_date(
        date_breaks = "1 day",         
        date_labels = "%d %b"
      ) +
      labs(title = "Погодные параметры (средние за 7 предыдущих дней)", x = "Дата", y = "") +
      theme_minimal() +
      theme(
        plot.margin = margin(10, 20, 10, 10)  
      )
    
    # Сохраняем график в файл
    dir.create("output", showWarnings = FALSE, recursive = TRUE)
    ggsave("output/plot.png", plot = p, width = 10, height = 6)
    
    return(p)
  } else {
    message("Ошибка! Данные пустые или отсутствуют!")
    return(NULL)
  }
}