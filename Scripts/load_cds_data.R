# Загрузка метеоданных ERA5 с Climate Data Store (CDS)
# Загружает погодные переменные за 7 дней, начиная с 12 дней назад, в формате NetCDF.
load_cds_data <- function(user_id = Sys.getenv("CDS_USER_ID"),
                          api_key = Sys.getenv("CDS_API_KEY")) {
  # Проверка наличия ключей API
  if (user_id == "" || api_key == "") {
    stop("❌ Переменные CDS_USER_ID или CDS_API_KEY не заданы.")
  }

  # Установка API-ключа для доступа к CDS
  ecmwfr::wf_set_key(user = user_id, key = api_key)

  # Папка для сохранения данных
  data_dir <- file.path(getwd(), "data")
  if (!dir.exists(data_dir)) {
    dir.create(data_dir, recursive = TRUE)
    message("✅ Создана директория 'data'")
  }

  # Период запроса: 7 дней, начиная с 12 дней назад
  start_date <- Sys.Date() - 12
  end_date <- start_date + 6
  dates_seq <- seq.Date(from = start_date, to = end_date, by = "day")

  # Название zip-файла для сохранения архива
  zip_name <- paste0("era5_data_", format(start_date, "%Y%m%d"),
                     "_to_", format(end_date, "%Y%m%d"), ".zip")
  zip_path <- file.path(tempdir(), zip_name)

  # Запрос на CDS API
  request <- list(
    dataset_short_name = "reanalysis-era5-land",       # Набор данных
    product_type = "reanalysis",
    variable = c(                                     # Переменные для анализа
      "10m_u_component_of_wind",                      # Компоненты ветра
      "10m_v_component_of_wind",
      "2m_temperature",                               # Температура воздуха
      "total_precipitation",                          # Осадки
      "surface_solar_radiation_downwards",            # Солнечная радиация
      "soil_temperature_level_1",                     # Температура почвы
      "soil_temperature_level_2",
      "soil_temperature_level_3",
      "leaf_area_index_high_vegetation",              # Индексы LAI
      "leaf_area_index_low_vegetation"
    ),
    year = format(start_date, "%Y"),
    month = format(start_date, "%m"),
    day = format(dates_seq, "%d"),                    # Список дней
    time = sprintf("%02d:00", 0:23),                  # Все часы
    area = c(56.5, 108, 49, 120),                     # BBox: North, West, South, East
    format = "netcdf",                                # Формат: .nc
    target = zip_name                                 # Имя файла
  )

  # Запрос и обработка
  tryCatch({
    # Отправка запроса
    result <- ecmwfr::wf_request(request = request, user = user_id)

    message("✅ Данные успешно загружены: ", result)

    # Распаковка архива в data_dir
    unzip(result, exdir = data_dir)
    nc_files <- list.files(data_dir, pattern = "\\.nc$", full.names = TRUE)

    if (length(nc_files) == 0) {
      stop("❌ Нет .nc файлов после распаковки архива!")
    }

    message("📦 NetCDF файл(-ы) перемещены в папку: ", data_dir)
  }, error = function(e) {
    message("❌ Ошибка загрузки ERA5: ", e$message)
  })
}
