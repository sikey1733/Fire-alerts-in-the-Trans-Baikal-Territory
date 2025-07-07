# Функция построения интерактивной карты
leaflet_vizualization_fire <- function(data) {
  if (!is.null(data) && nrow(data) > 0) {
    message("Строю интерактивный график!")
    
    if (!is.factor(data$confidence)) {
      data$confidence <- factor(data$confidence)
    }
    
    map <- leaflet(data %>% mutate(region_name = ifelse(is.na(region_name), "Неизвестно", region_name))) %>%
      addTiles() %>%
      addCircleMarkers(
        radius = ~sqrt(frp) * 2,
        color = ~colorFactor("Set1", confidence)(confidence),
        fillOpacity = 0.2,
        stroke = FALSE,
        popup = ~paste(
          "Мощность пожара: ", frp, "МВт<br>",
          "Дата обнаружения: ", acq_date, "<br>",
          "Время обнаружения: ", substr(acq_time, 1, 2), ":", substr(acq_time, 3, 4), "<br>",
          "Температура: ", round(as.numeric(bright_ti4) - 273.15, 1), "°C"
        )
      ) %>% 
      addLegend(
        pal = colorFactor("Set1", domain = data$confidence),
        values = data$confidence,
        title = "Уровень доверия",
        labFormat = labelFormat(
          transform = function(x) {
            ifelse(x == "h", "Высокий", 
                   ifelse(x == "n", "Средний", "Неизвестно"))
          }
        )
      )
    return(map)
  } else {
    message("Данных нет!")
    return(NULL)
  }
}