# Главная функция фильтрации пожаров по регионам
filter_fires_by_region <- function(csv_data_dir = "data/",
                                   polygon_path = "data/polygons.gpkg") {
  fire_df <- filter_data_one(csv_data_dir)
  if (is.null(fire_df)) return(NULL)
  
  fire_df <- fire_df %>%
    filter(confidence %in% c("n", "h"))
  
  if (nrow(fire_df) == 0) {
    message("Нет точек с высоким или нормальным доверием.")
    return(NULL)
  }
  region_names <- c(
    "Забайкальский край, Россия",
    "Республика Бурятия, Россия",
    "Амурская область, Россия",
    "Иркутская область, Россия"
  )
  region_poly <- request_polygon(region_names, save_path = polygon_path)
  if (is.null(region_poly)) return(NULL)
  
  message("Полигоны получены!")
  
  
  fires_sf <- st_as_sf(fire_df, coords = c("longitude", "latitude"), crs = 4326)
  message("Фильтрация точек по полигонам...")
  inside <- st_within(fires_sf, region_poly, sparse = FALSE)
  filtered <- fires_sf[apply(inside, 1, any), ]
  message("За прошлый день найдено ", nrow(filtered), " точек внутри указанных регионов.")
  return(filtered)
}
