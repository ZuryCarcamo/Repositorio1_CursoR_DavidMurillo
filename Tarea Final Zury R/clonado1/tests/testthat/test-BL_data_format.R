library(tidyverse)


datapackage_path <- "../data/datapackage.json"
breeding_status_path <- "../data/breeding_status_albatross_guadalupe.csv"
tracking_path <- "../data/gps-albatros-guadalupe.csv"

describe("write_bl_table", {
  it("construct_bl_table ", {
    relative_path <- paste0(getwd(), "/../data/")
    setwd(relative_path)
    output_path <- "bl_albatross_guadalupe.csv"
    datapackage_path <- "datapackage.json"
    write_bl_table(datapackage_path, output_path)
    expect_true(testtools::exist_output_file(output_path))
  })
})

describe("Construct BL table", {
  it("construct_bl_table ", {
    obtained_bl_table <- construct_bl_table(breeding_status_path, tracking_path, datapackage_path)
    expected_columns <- c(
      "bird_id",
      "track_id",
      "original_track_id",
      "sex",
      "lat_colony",
      "lon_colony",
      "breed_stage",
      "breed_status",
      "date_gmt",
      "time_gmt",
      "longitude",
      "latitude",
      "argos_quality",
      "common_name",
      "site_name",
      "colony_name",
      "device"
    )
    obtained_columns <- colnames(obtained_bl_table)
    expect_true(all(expected_columns %in% obtained_columns))
  })
})
describe("Join data columns", {
  breeding_status <- read_csv(breeding_status_path, show_col_types = FALSE)
  tracking_data <- read_csv(tracking_path, show_col_types = FALSE)
  obtained <- join_seabird_breeding_status_with_tracking_data(breeding_status, tracking_data)
  it("Check columns", {
    expected_columns <- c(
      "bird_id",
      "track_id",
      "original_track_id",
      "sex",
      "lat_colony",
      "lon_colony",
      "breed_stage",
      "breed_status",
      "date_gmt",
      "time_gmt",
      "longitude",
      "latitude",
      "argos_quality"
    )
    obtained_columns <- colnames(obtained)
    expect_true(all(obtained_columns %in% expected_columns))
    expected_number_columns <- length(expected_columns)
    obtained_number_columns <- length(obtained_columns)
    expect_equal(obtained_number_columns, expected_number_columns)
  })
  it("Check rows", {
    obtained_rows_LAAL01 <- nrow(filter(obtained, bird_id == "LAAL01"))
    expected_rows_LAAL01 <- 3
    expect_equal(obtained_rows_LAAL01, expected_rows_LAAL01)
    obtained_rows <- nrow(obtained)
    expected_rows <- 29
    expect_equal(obtained_rows, expected_rows)
  })
})
describe("Fill metadata data columns", {
  it("Check columns", {
    expected_columns <- c(
      "common_name",
      "site_name",
      "colony_name",
      "device"
    )
    obtained_metadata <- get_metadata(datapackage_path)
    obtained_columns <- colnames(obtained_metadata)
    expect_equal(obtained_columns, expected_columns)
    expected_metadata <- tibble(common_name = "Laysan albatross", site_name = "Mexico", colony_name = "Isla Guadalupe", device = "GPS")
    expect_equal(obtained_metadata, expected_metadata)
  })
  it("Test read new datapackage", {
    datapackage_path <- "../data/datapackage_pardela.json"
    resource_name <- "breeding_status_pardela_honolulu"
    obtained <- get_metadata(datapackage_path, resource_name = resource_name)
    obtained_site_name <- obtained$site_name[[1]]
    expected_site_name <- "USA"
    expect_equal(obtained_site_name, expected_site_name)
  })
  it("Test read metadata from gps-albatross", {
    resource_name <- "gps-albatros-guadalupe"
    obtained_metadata <- get_metadata_path(datapackage_path, resource_name = resource_name)
    obtained_path <- obtained_metadata
    expected_path <- "gps-albatros-guadalupe.csv"
    expect_equal(obtained_path, expected_path)
  })
})
