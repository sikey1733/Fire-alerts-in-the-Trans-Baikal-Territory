main <- function() {
  message("🚀 Запуск обработки данных...")

  # 📦 Список всех необходимых пакетов (без mapview)
  required_packages <- c(
    "magrittr", "dplyr", "ecmwfr", "stars", "sf", "units",
    "lubridate", "httr", "leaflet", "geosphere", "osmdata",
    "htmlwidgets", "ggplot2", "webshot"
  )

  # 📦 Унифицированная установка и загрузка
  install_and_load <- function(pkg) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      message(paste0("📦 Устанавливаю пакет ", pkg, "..."))
      tryCatch(
        install.packages(pkg, repos = "https://cloud.r-project.org"),
        error = function(e) {
          message(paste0("❌ Ошибка установки пакета ", pkg, ": ", e$message))
          stop("Прерываю выполнение.")
        }
      )
    }
    library(pkg, character.only = TRUE)
  }

  # 🔁 Установка и загрузка всех
  invisible(lapply(required_packages, install_and_load))

  # 📂 Загрузка всех R-скриптов
  script_paths <- list.files("Scripts", full.names = TRUE, pattern = "\\.R$")
  lapply(script_paths, source)

  # 🚀 Основной конвейер
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

  write(paste(Sys.time(), "✅ Успешно завершено"), file = "last_success.log", append = TRUE)
}

main()
