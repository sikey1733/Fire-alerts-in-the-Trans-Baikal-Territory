main <- function() {
  # --- Защита от повторного запуска (рекурсии) ---
  if (exists("processing_in_progress", envir = .GlobalEnv) && get("processing_in_progress", envir = .GlobalEnv)) {
    message("Скрипт уже запущен — прерываю, чтобы избежать рекурсии.")
    return(invisible(NULL))
  }
  assign("processing_in_progress", TRUE, envir = .GlobalEnv)

  message("Запуск обработки данных...")

  # --- Список обязательных пакетов ---
  required_packages <- c(
    "magrittr", "dplyr", "ecmwfr", "stars", "sf", "units",
    "lubridate", "httr", "geosphere", "osmdata", "ggplot2",
    "maptiles", "terra", "ggspatial", "prettymapr"
  )

  # Функция проверки и установки пакета
  install_and_load <- function(pkg) {
    message("Проверка пакета: ", pkg)
    if (!requireNamespace(pkg, quietly = TRUE)) {
      message("Установка пакета: ", pkg)
      tryCatch({
        install.packages(pkg, repos = "https://cloud.r-project.org")
      }, error = function(e) {
        message("Ошибка установки пакета ", pkg, ": ", e$message)
        stop("Прерываю выполнение.")
      })
    }
    # Загрузка пакета без сообщений
    success <- suppressPackageStartupMessages(require(pkg, character.only = TRUE))
    if (!success) {
      stop("Не удалось загрузить пакет: ", pkg)
    }
  }

  # Проверяем и ставим все нужные пакеты
  for (pkg in required_packages) {
    install_and_load(pkg)
  }
  message("Все пакеты установлены и загружены.")

  # --- Загрузка всех скриптов из папки Scripts ---
  script_paths <- list.files("Scripts", full.names = TRUE, pattern = "\\.R$")
  if (length(script_paths) == 0) {
    stop("Нет .R скриптов в папке Scripts.")
  }
  
  for (script in script_paths) {
    message("Загружаю скрипт: ", basename(script))
    tryCatch({
      source(script)
      message("Успешно загружен: ", basename(script))
    }, error = function(e) {
      message("Ошибка загрузки скрипта ", basename(script), ": ", e$message)
      stop("Прерываю выполнение.")
    })
  }
  message("Все скрипты загружены.")

  # --- Задаём регионы для анализа ---
  region_names <- c(
    "Забайкальский край, Россия",
    "Республика Бурятия, Россия",
    "Амурская область, Россия",
    "Иркутская область, Россия"
  )

  # --- Последовательное выполнение основных шагов ---
  message("Шаг 1: Загрузка ERA5 данных")
  load_cds_data()
  message("Шаг 1 завершён")

  message("Шаг 2: Чтение .nc файла")
  weather_data <- read_file_nc()
  if (is.null(weather_data)) {
    message(" Шаг 2: Ошибка чтения .nc файла")
    assign("processing_in_progress", FALSE, envir = .GlobalEnv)
    return()
  }
  message(" Шаг 2 завершён")

  message("Шаг 3: Трансформация данных")
  transformed_data <- transform_data_nc(weather_data)
  if (is.null(transformed_data)) {
    message("Шаг 3: Ошибка трансформации")
    assign("processing_in_progress", FALSE, envir = .GlobalEnv)
    return()
  }
  message("Шаг 3 завершён")

  message("Шаг 4: Очистка пропущенных значений")
  cleaned_data <- clear_na_nc(transformed_data)
  if (is.null(cleaned_data)) {
    message("Шаг 4: Ошибка очистки данных")
    assign("processing_in_progress", FALSE, envir = .GlobalEnv)
    return()
  }
  message("Шаг 4 завершён")

  message("Шаг 5: Загрузка данных VIIRS NOAA21")
  download_viirs_noaa21_375m()
  message(" Шаг 5 завершён")

  # 6. Фильтрация пожаров по регионам
  fire_data <- filter_fires_by_region(region_names = region_names)
  if (is.null(fire_data)) {
    message("Шаг 6: Ошибка фильтрации пожаров")
    assign("processing_in_progress", FALSE, envir = .GlobalEnv)
    return()
  }
  message("Шаг 6 завершён")
  
  # 7. Расчёт расстояний до объектов
  fire_with_distances <- calculate_fire_distances(fire_sf = fire_data, region_names = region_names)
  if (is.null(fire_with_distances)) {
    message("Шаг 7: Ошибка расчёта расстояний")
    assign("processing_in_progress", FALSE, envir = .GlobalEnv)
    return()
  }
  message("Шаг 7 завершён")

  message("Шаг 8: Загрузка населённых пунктов")
  places_sf <- get_all_places(region_names)
  if (is.null(places_sf)) {
    message("Шаг 8: Ошибка загрузки населённых пунктов")
    assign("processing_in_progress", FALSE, envir = .GlobalEnv)
    return()
  }
  message("Шаг 8 завершён")

  message(" Шаг 9: Загрузка водоёмов")
  water_sf <- get_all_waterbodies(region_names)
  if (is.null(water_sf)) {
    message("Шаг 9: Ошибка загрузки водоёмов")
    assign("processing_in_progress", FALSE, envir = .GlobalEnv)
    return()
  }
  message("Шаг 9 завершён")

  message("Шаг 10: Отправка уведомления Telegram")
  filter_and_notify(fire_with_distances, cleaned_data)
  message("Шаг 10 завершён")

  # Логируем успешное завершение
  write(paste(Sys.time(), "Успешно завершено"), file = "last_success.log", append = TRUE)

  # Сбрасываем флаг выполнения
  assign("processing_in_progress", FALSE, envir = .GlobalEnv)
}

# Запуск главной функции
main()

# Вывод предупреждений, если есть
warnings()
