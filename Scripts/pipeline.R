main <- function() {
  message("🚀 Запуск обработки данных...")

  # Пакеты и загрузка скриптов — как у тебя

  region_names <- c(
    "Забайкальский край, Россия",
    "Республика Бурятия, Россия",
    "Амурская область, Россия",
    "Иркутская область, Россия"
  )

  load_cds_data()
  weather_data <- read_file_nc()
  if (is.null(weather_data)) return()

  transformed_data <- transform_data_nc(weather_data)
  if (is.null(transformed_data)) return()

  cleaned_data <- clear_na_nc(transformed_data)
  if (is.null(cleaned_data)) return()

  download_viirs_noaa21_375m()
  fire_data <- filter_fires_by_region(region_names = region_names)
  if (is.null(fire_data)) return()

  fire_with_distances <- calculate_fire_distances(region_names = region_names)
  if (is.null(fire_with_distances)) return()

  places_sf <- get_all_places(region_names)
  water_sf <- get_all_waterbodies(region_names)

  leaflet_nearest_fire_map(fire_with_distances, places_sf, water_sf)

  filter_and_notify(fire_with_distances)

  write(paste(Sys.time(), "✅ Успешно завершено"), file = "last_success.log", append = TRUE)
}
