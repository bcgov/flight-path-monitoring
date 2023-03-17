test_that("Time in zone returns errors when expected and computes the time spent in zones correctly.", {

  # Create a LINESTRING sf object
  sf_obj <- c(-5,5,0,0) |> matrix(2,2) |> sf::st_linestring() |> sf::st_sfc() |> sf::st_as_sf()
  sf_obj[["time_deltas"]] <- as.difftime(10, format = "%X", units = "secs", tz = "UTC")

  # Create a list of named zones
  zones <- list(
    "2" = c(0,0) |> sf::st_point() |> sf::st_buffer(1) |> sf::st_sfc(),
    "4" = c(0,0) |> sf::st_point() |> sf::st_buffer(2) |> sf::st_sfc(),
    "6" = c(0,0) |> sf::st_point() |> sf::st_buffer(3) |> sf::st_sfc(),
    "8" = c(0,0) |> sf::st_point() |> sf::st_buffer(4) |> sf::st_sfc()
  )

  sf_obj[["full_length"]] <- sf::st_length(sf_obj)
  sf::st_agr(sf_obj) <- factor("constant", levels(sf::st_agr(sf_obj)))

  options("flight.path.monitoring.use.parallel" = FALSE)
  res <- time_in_zone(sf_obj, zones)

  # Results checks
  expect_equal(c(names(zones), "all"), names(res[["time_in_zones"]]))
  expect_equal(names(zones), names(res[["segments_in_zones"]]))
  expect_equal(res[["segments_in_zones"]][["2"]], sf::st_intersection(sf_obj, zones[["2"]]))
  expect_equal(res[["segments_in_zones"]][["4"]], sf::st_intersection(sf_obj, zones[["4"]]))
  expect_equal(res[["segments_in_zones"]][["6"]], sf::st_intersection(sf_obj, zones[["6"]]))
  expect_equal(res[["segments_in_zones"]][["8"]], sf::st_intersection(sf_obj, zones[["8"]]))
  expect_equal(as.integer(res[["time_in_zones"]]), c(1:4,10)*2)

  # Empty checks
  sf_obj_empty <- sf::st_linestring() |> sf::st_sfc() |> sf::st_as_sf()
  sf_obj_empty[["time_deltas"]] <- as.difftime(integer(1), format = "%X", units = "secs", tz = "UTC")
  expect_silent(time_in_zone(sf_obj_empty, setNames(list(), character())))

  # Errors checks
  sf_obj[["time_deltas"]] <- NULL
  expect_error(time_in_zone(sf_obj, zones), '"time_deltas" %in% names(sf_obj) is not TRUE', fixed = TRUE)
  names(zones) <- NULL
  expect_error(time_in_zone(sf_obj, zones), '!is.null(names(zones)) is not TRUE', fixed = TRUE)
  zones[[3]] <- list()
  expect_error(time_in_zone(sf_obj, zones), 'all(vapply(zones, inherits, logical(1), c("sfc_MULTIPOLYGON",', fixed = TRUE)
  sf_obj <- c(0,0) |> sf::st_point() |> sf::st_sfc() |> sf::st_as_sf()
  expect_error(time_in_zone(sf_obj, zones), 'inherits(sf::st_geometry(sf_obj), "sfc_LINESTRING") is not TRUE', fixed = TRUE)
  sf_obj <- list()
  expect_error(time_in_zone(sf_obj, zones), 'inherits(sf_obj, "sf") is not TRUE', fixed = TRUE)

  options("flight.path.monitoring.maxcores" = NULL)

})
