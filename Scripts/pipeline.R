# Загрузка всех функций
source("scripts/load_cds_data.R")
source("scripts/read_file_nc.R")
source("scripts/transform_data_nc.R")
source("scripts/clear_na_nc.R")
source("scripts/download_viirs_noaa21_375m.R")
source("scripts/filter_fires_by_region.R")
source("scripts/calculate_fire_distances.R")
source("scripts/get_all_places.R")
source("scripts/get_all_waterbodies.R")
source("scripts/leaflet_nearest_fire_map.R")
source("scripts/filter_and_notify.R")
source("scripts/send_telegram_message.R")
source("scripts/send_telegram_image.R")
source("scripts/filter_critical_fires_dynamic.R")
source("scripts/calc_fire_risk_flag.R")

# Главная функция
main <- function() {
  message("🚀 Запуск обработки данных...")

  load_cds_data()
  weather_data <- read_file_nc()
  if (is.null(weather_data)) return()

  transformed_data <- transform_data_nc(weather_data)
  if (is.null(transformed_data)) return()

  cleaned_data <- clear_na_nc(transformed_data)
  if (is.null(cleaned_data)) return()

  download_viirs_noaa21_375m()
  fire_data <- filter_fires_by_region()
  if (is.null(fire_data)) return()

  fire_with_distances <- calculate_fire_distances()
  if (is.null(fire_with_distances)) return()

  region_names <- c(
    "Забайкальский край, Россия",
    "Республика Бурятия, Россия",
    "Амурская область, Россия",
    "Иркутская область, Россия"
  )
  places_sf <- get_all_places(region_names)
  water_sf <- get_all_waterbodies(region_names)
  leaflet_nearest_fire_map(fire_with_distances, places_sf, water_sf)

  filter_and_notify(cleaned_data)

  write(paste(Sys.time(), "успешно завершено"), file = "last_success.log", append = TRUE)
}

main()
