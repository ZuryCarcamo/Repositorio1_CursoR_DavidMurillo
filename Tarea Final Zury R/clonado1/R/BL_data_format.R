#' @import dplyr
#' @import readr

#' @export
write_bl_table <- function(datapackage_path = "datapackage.json", output_path = "bl_gps_albatross_guadalupe.csv") {
  breeding_status_path <- get_metadata_path(datapackage_path, "breeding_status_albatross_guadalupe")
  tracking_path <- get_metadata_path(datapackage_path, "gps-albatros-guadalupe")
  bl_table <- construct_bl_table(breeding_status_path, tracking_path, datapackage_path)
  write_csv(bl_table, output_path)
}

construct_bl_table <- function(breeding_status_path, tracking_path, datapackage_path) {
  breeding_status <- read_csv(breeding_status_path, show_col_types = FALSE)
  tracking <- read_csv(tracking_path, show_col_types = FALSE)
  data_table <- join_seabird_breeding_status_with_tracking_data(breeding_status, tracking)
  metadata <- get_metadata(datapackage_path)
  data_table %>% mutate(metadata)
}

join_seabird_breeding_status_with_tracking_data <- function(breeding_status, tracking_data) {
  right_join(breeding_status, tracking_data, by = c("bird_id" = "name"), multiple = "all") |>
    rename(date_gmt = date, lat_colony = nest_lat, lon_colony = nest_lon) |>
    mutate(track_id = bird_id, original_track_id = bird_id, time_gmt = NA, argos_quality = NA)
}

get_metadata <- function(datapackage_path, resource_name = "breeding_status_albatross_guadalupe") {
  resource <- get_resource(datapackage_path, resource_name)
  tibble(
    common_name = resource$common_name,
    site_name = resource$site_name,
    colony_name = resource$colony_name,
    device = resource$device
  )
}

get_metadata_path <- function(datapackage_path, resource_name = resource_name) {
  resource <- get_resource(datapackage_path, resource_name)
  resource$path[[1]]
}

get_resource <- function(datapackage_path, resource_name) {
  datapackage <- rjson::fromJSON(file = datapackage_path, simplify = FALSE)
  resource_index <- which(sapply(datapackage$resources, function(x) resource_name %in% x))
  resource <- datapackage$resources[[resource_index]]
}
