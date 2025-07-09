# Запрос CDS
load_cds_data <- function(user_id = Sys.getenv("CDS_USER_ID"),
                          api_key = Sys.getenv("CDS_API_KEY")) {
  if (user_id == "" || api_key == "") {
    stop("❌ Переменные CDS_USER_ID или CDS_API_KEY не заданы.")
  }

  # Установка ключа для ecmwfr (без service = "cds")
  ecmwfr::wf_set_key(user = user_id, key = api_key)

  data_dir <- file.path(getwd(), "data")
  if (!dir.exists(data_dir)) {
    dir.create(data_dir)
    message("✅ Создана директория 'data'")
  }

  start_date <- Sys.Date() - 12
  end_date <- start_date + 6
  dates_seq <- seq.Date(from = start_date, to = end_date, by = "day")

  file_name <- paste0("era5_data_", format(start_date, "%Y%m%d"),
                      "_to_", format(end_date, "%Y%m%d"), ".nc")
  target_path <- file.path(data_dir, file_name)

  request <- list(
    dataset_short_name = "reanalysis-era5-single-levels",
    product_type = "reanalysis",
    variable = c(
      "10m_u_component_of_wind",
      "10m_v_component_of_wind",
      "2m_temperature",
      "total_precipitation",
      "surface_solar_radiation_downwards",
      "soil_temperature_level_1",
      "soil_temperature_level_2",
      "soil_temperature_level_3",
      "leaf_area_index_high_vegetation",
      "leaf_area_index_low_vegetation"
    ),
    year = format(start_date, "%Y"),
    month = format(start_date, "%m"),
    day = format(dates_seq, "%d"),
    time = sprintf("%02d:00", 0:23),
    area = c(56.5, 108, 49, 120),  # N, W, S, E
    format = "netcdf",
    target = file_name
  )

  tryCatch({
    result <- ecmwfr::wf_request(request = request, user = user_id)
    message("✅ Данные успешно загружены: ", result)
    file.rename(result, target_path)
    message("📦 Файл перемещён в: ", target_path)
  }, error = function(e) {
    message("❌ Ошибка загрузки ERA5: ", e$message)
  })
}
