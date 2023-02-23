test_that("Contiguous points are dropped and fields are kept", {

  sf_obj <- sf::st_sfc(
    st_point(c(0,1)),
    st_point(c(0,1)),
    st_point(c(1,1)),
    st_point(c(1,2)),
    st_point(c(1,2)),
    st_point(c(2,2)),
    st_point(c(0,1))
  ) |> sf::st_as_sf()

  sf_obj2 <- sf::st_sfc(
    st_point(c(0,1)),
    st_point(c(1,1)),
    st_point(c(1,2)),
    st_point(c(2,2)),
    st_point(c(0,1))
  ) |> sf::st_as_sf()

  res <- remove_contiguous_duplicates(sf_obj)

  # Check results
  expect_equal(res, sf_obj2)

  # Check errors
  sf_obj <- c(-5,5,0,0) |> matrix(2,2) |> sf::st_linestring() |> sf::st_sfc() |> sf::st_as_sf()
  expect_error(remove_contiguous_duplicates(sf_obj), 'inherits(sf::st_geometry(sf_obj), "sfc_POINT") is not TRUE', fixed = TRUE)
  sf_obj <- list()
  expect_error(remove_contiguous_duplicates(sf_obj), 'inherits(sf_obj, "sf") is not TRUE', fixed = TRUE)

})
