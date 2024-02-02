#' @export
clean_gps_from_txt <- function(raw_path, bird_id, output_path) {
  extracted_points <- extract_points(raw_path)
  points_with_id <- extracted_points |>
    dplyr::mutate(bird_id = bird_id) |>
    dplyr::mutate(Date = lubridate::dmy(Date))
  readr::write_csv(points_with_id, output_path)
}

extract_points <- function(raw_path) {
  temporal_file <- "tmp.txt"
  command <- glue::glue("cat {raw_path} | grep --invert-match 'EVENT' > {temporal_file}")
  system(command)
  columns_of_interest <- c("Date", "Time", "Longitude", "Latitude")
  raw <- readr::read_tsv(temporal_file, col_names = columns_of_interest, show_col_types = FALSE) |>
    dplyr::select(dplyr::all_of(columns_of_interest))
  return(raw)
}
