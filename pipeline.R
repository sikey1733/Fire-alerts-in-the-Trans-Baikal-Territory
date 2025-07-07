main <- function() {
  message("🚀 Запуск обработки данных...")

  # 1. Загрузка погодных данных ERA5
  load_cds_data()

  # 2. Чтение и объединение файлов
  weather_data <- read_file_nc()
  if (is.null(weather_data)) return()

  # 3. Трансформация
  transformed_data <- transform_data_nc(weather_data)
  if (is.null(transformed_data)) return()

  # 4. Очистка от NA
  cleaned_data <- clear_na_nc(transformed_data)
  if (is.null(cleaned_data)) return()

  # 5. Визуализация графиков
  vizualization_weather_param(cleaned_data)
  roza_air_vizualization(cleaned_data)

  # 6. Загрузка VIIRS пожаров
  download_viirs_noaa21_375m()

  # 7. Фильтрация пожаров по регионам
  fire_data <- filter_fires_by_region()
  if (is.null(fire_data)) return()

  # 8. Интерактивная карта всех пожаров
  leaflet_vizualization_fire(fire_data)

  # 9. Расчёт расстояний до населённых пунктов и водоёмов
  fire_with_distances <- calculate_fire_distances()
  if (is.null(fire_with_distances)) return()

  # 10. Карта ближайшего пожара, поселения и водоёма
  region_names <- c(
    "Забайкальский край, Россия",
    "Республика Бурятия, Россия",
    "Амурская область, Россия",
    "Иркутская область, Россия"
  )
  places_sf <- get_all_places(region_names)
  water_sf <- get_all_waterbodies(region_names)
  leaflet_nearest_fire_map(fire_with_distances, places_sf, water_sf)

  # 11. Отправка уведомлений в Telegram
  filter_and_notify(cleaned_data)
}
