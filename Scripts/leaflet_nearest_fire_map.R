# Функция для построения интерактивной карты ближайшего пожара, населённого пункта и водоёма:
leaflet_nearest_fire_map <- function(fires_sf, places_sf, water_sf) {
  if (is.null(fires_sf) || nrow(fires_sf) == 0) {
    message("Нет данных о пожарах")
    return(NULL)
  }
  
  nearest_fire <- fires_sf %>%
    filter(distance_to_settlement_km == min(distance_to_settlement_km, na.rm = TRUE)) %>%
    slice(1)
  
  nearest_place_name <- nearest_fire$settlement_name
  nearest_place <- places_sf %>% filter(name == nearest_place_name)
  
  fire_geom <- st_geometry(nearest_fire)
  water_sf$dist_to_fire <- as.numeric(st_distance(water_sf, fire_geom))
  nearest_water <- water_sf[which.min(water_sf$dist_to_fire), ]
  
  message("Строю интерактивную карту ближайшего водоема, населёного пункта и возгорания!")
  
  map <- leaflet() %>%
    addTiles() %>%
    addCircleMarkers(data = nearest_fire, color = "red", radius = 10, popup = "Ближайшее возгорание") %>%
    addCircleMarkers(data = nearest_place, color = "blue", radius = 7, popup = paste("Населённый пункт:", nearest_place_name)) %>%
    addPolygons(data = nearest_water, color = "cyan", weight = 2, fillOpacity = 0.3, popup = "Ближайший водоём") %>%
    addLegend(colors = c("red", "blue", "cyan"), labels = c("Пожар", "Населённый пункт", "Водоём"), opacity = 0.8)
  
  dir.create("output", showWarnings = FALSE, recursive = TRUE)
  htmlwidgets::saveWidget(map, "output/nearest_fire_map.html", selfcontained = TRUE)
  webshot("output/nearest_fire_map.html", file = "output/nearest_fire_map.png", vwidth = 800, vheight = 600)
  
  return(map)
}