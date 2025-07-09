main <- function() {
  message("🚀 Запуск обработки данных...")

  # 📦 Список всех необходимых пакетов
  required_packages <- c(
    "magrittr", "dplyr", "ecmwfr", "stars", "sf", "units",
    "lubridate", "httr", "leaflet", "geosphere", "osmdata",
    "htmlwidgets", "ggplot2", "webshot"
  )

  # 📦 Функция установки и загрузки одного пакета
  install_and_load <- function(pkg) {
    message("🔄 Проверка пакета: ", pkg)
    if (!requireNamespace(pkg, quietly = TRUE)) {
      message("📦 Установка пакета: ", pkg)
      tryCatch({
        install.packages(pkg, repos = "https://cloud.r-project.org")
      }, error = function(e) {
        message("❌ Ошибка установки пакета ", pkg, ": ", e$message)
        stop("Прерываю выполнение.")
      })
    }
    # Используем require вместо library, чтобы избежать stack overflow
    suppressPackageStartupMessages(
      if (!require(pkg, character.only = TRUE)) {
        stop("❌ Не удалось загрузить пакет: ", pkg)
      }
    )
  }

  # 🔁 Проверка и установка всех пакетов
  for (pkg in required_packages) {
    install_and_load(pkg)
  }
  message("✅ Все пакеты установлены и загружены.")

  # 📂 Загрузка всех R-скриптов
  script_paths <- list.files("Scripts", full.names = TRUE, pattern = "\\.R$")
  if (length(script_paths) == 0) {
    stop("❌ Нет .R скриптов в папке Scripts.")
  }
  lapply(script_paths, source)
  message("✅ Все скрипты загружены.")

  # 📌 Задание регионов
  region_names <- c(
    "Забайкальский край, Россия",
    "Республика Бурятия, Россия",
    "Амурская область, Россия",
    "Иркутская область, Россия"
  )

  # 🚀 Основной пайплайн
  load_cds_data()
  message("✅ Шаг 1: ERA5 загружены.")

  weather_data <- read_file_nc()
  if (is.null(weather_data)) {
    message("❌ Шаг 2: Ошибка чтения .nc файла")
    return()
  }
  message("✅ Шаг 2: Прочитан .nc файл")

  transformed_data <- transform_data_nc(weather_data)
  if (is.null(transformed_data)) {
    message("❌ Шаг 3: Ошибка трансформации")
    return()
  }
  message("✅ Шаг 3: Трансформация погоды")

  cleaned_data <- clear_na_nc(transformed_data)
  if (is.null(cleaned_data)) {
    message("❌ Шаг 4: Ошибка очистки данных")
    return()
  }
  message("✅ Шаг 4: Очистка пропущенных значений")

  download_viirs_noaa21_375m()
  message("✅ Шаг 5: VIIRS NOAA21 загружен")

  fire_data <- filter_fires_by_region(region_names = region_names)
  if (is.null(fire_data)) {
    message("❌ Шаг 6: Ошибка фильтрации пожаров")
    return()
  }
  message("✅ Шаг 6: Пожары отфильтрованы по регионам")

  fire_with_distances <- calculate_fire_distances(region_names = region_names)
  if (is.null(fire_with_distances)) {
    message("❌ Шаг 7: Ошибка расчёта расстояний")
    return()
  }
  message("✅ Шаг 7: Расстояния до объектов рассчитаны")

  places_sf <- get_all_places(region_names)
  if (is.null(places_sf)) {
    message("❌ Шаг 8: Не удалось загрузить населённые пункты")
    return()
  }
  message("✅ Шаг 8: Населённые пункты загружены")

  water_sf <- get_all_waterbodies(region_names)
  if (is.null(water_sf)) {
    message("❌ Шаг 9: Не удалось загрузить водоёмы")
    return()
  }
  message("✅ Шаг 9: Водоёмы загружены")

  leaflet_nearest_fire_map(fire_with_distances, places_sf, water_sf)
  message("✅ Шаг 10: Карта построена и сохранена")

  filter_and_notify(fire_with_distances)
  message("✅ Шаг 11: Telegram-уведомление отправлено")

  write(paste(Sys.time(), "✅ Успешно завершено"), file = "last_success.log", append = TRUE)
}

# Запуск
main()
