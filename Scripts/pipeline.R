main <- function() {
  message("🚀 Запуск обработки данных...")

  # ... установка пакетов и скриптов ...

  region_names <- c(
    "Забайкальский край, Россия",
    "Республика Бурятия, Россия",
    "Амурская область, Россия",
    "Иркутская область, Россия"
  )

  load_cds_data()
  message("✅ Шаг 1: ERA5 загружены.")

  weather_data <- read_file_nc()
  if (is.null(weather_data)) return()
  message("✅ Шаг 2: Прочитан .nc файл")

  transformed_data <- transform_data_nc(weather_data)
  if (is.null(transformed_data)) return()
  message("✅ Шаг 3: Трансформация погоды")

  cleaned_data <- clear_na_nc(transformed_data)
  if (is.null(cleaned_data)) return()
  message("✅ Шаг 4: Очистка пропущенных")

  download_viirs_noaa21_375m()
  message("✅ Шаг 5: VIIRS NOAA21 загружен")

  fire_data <- filter_fires_by_region(region_names = region_names)
  if (is.null(fire_data)) return()
  message("✅ Шаг 6: Пожары отфильтрованы по регионам")

  fire_with_distances <- calculate_fire_distances(region_names = region_names)
  if (is.null(fire_with_distances)) return()
  message("✅ Шаг 7: Расстояния рассчитаны")

  places_sf <- get_all_places(region_names)
  message("✅ Шаг 8: Населённые пункты загружены")

  water_sf <- get_all_waterbodies(region_names)
  message("✅ Шаг 9: Водоёмы загружены")

  leaflet_nearest_fire_map(fire_with_distances, places_sf, water_sf)
  message("✅ Шаг 10: Карта построена")

  filter_and_notify(fire_with_distances)
  message("✅ Шаг 11: Telegram-уведомление отправлено")

  write(paste(Sys.time(), "✅ Успешно завершено"), file = "last_success.log", append = TRUE)
}
