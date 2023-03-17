test_that("Flight can be read in both format and have the same elements", {

  options("flight.path.monitoring.use.parallel" = FALSE)
  dsn <- list.files(test_path("files"), full.names = TRUE)

  expect_warning(flights <- read_flight(dsn), "File extension not supported")

  expect_true(all(c("tracks", "track_points") %in% names(flights[["gpx_flight"]])))
  expect_true(all(c("tracks", "track_points") %in% names(flights[["kml_flight"]])))

  expect_true(all(c("track_seg_point_id", "ele", "geometry", "time") %in% names(flights[["gpx_flight"]][["track_points"]])))
  expect_true(all(c("track_seg_point_id", "ele", "geometry", "time") %in% names(flights[["gpx_flight"]][["track_points"]])))

  expect_equal("POINT", unique(sf::st_geometry_type(flights[["gpx_flight"]][["track_points"]])) |> as.character())
  expect_equal("MULTILINESTRING", unique(sf::st_geometry_type(flights[["gpx_flight"]][["tracks"]])) |> as.character())

  expect_equal("POINT", unique(sf::st_geometry_type(flights[["kml_flight"]][["track_points"]])) |> as.character())
  expect_equal("MULTILINESTRING", unique(sf::st_geometry_type(flights[["kml_flight"]][["tracks"]])) |> as.character())

})
