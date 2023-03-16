test_that("Process flight works", {
  options("flight.path.monitoring.use.parallel" = FALSE)
  expect_no_error({
    system.file("flight.tar.gz", package = "flight.path.monitoring") |>
      untar(exdir = tempdir())
    analysis <- file.path(tempdir(), "flight.gpx") |> read_flight() |> process_flight()
  })
})
