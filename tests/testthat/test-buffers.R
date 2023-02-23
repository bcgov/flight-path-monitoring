test_that("buffer distance names and sort correctly", {

  expect_equal(distances(64, 48, a = 32), c("In UWR" = 0, "a" = 32, "48" = 48, "64" = 64), ignore_attr = TRUE)
  expect_equal(distances(64, 48, a = 32, reflabel = "In b"), c("In b" = 0, "a" = 32, "48" = 48, "64" = 64), ignore_attr = TRUE)
  expect_error(distances(0, 48, a = 32, reflabel = "In b"), regexp = "all(dist > 0L) is not TRUE", fixed = TRUE)
  expect_error(distances(48, 48, a = 32, reflabel = "In b"), regexp = "all(!duplicated(dist)) is not TRUE", fixed = TRUE)
  expect_error(distances(48, 50, All = 32, reflabel = "In b"), regexp = 'all(!names(dist) %in% c("All", "Buffers")) is not TRUE', fixed = TRUE)

})

test_that("buffer compute correctly", {

  sf_obj <- sf::st_sfc(sf::st_point(c(3,4)), sf::st_point(c(10,11))) |> sf::st_as_sf()
  dist <- distances(10, 20, 30, reflabel = "0")

  res <- buffers(sf_obj, dist)

  expect_equal(names(res), c("0","10","20","30","Buffers","All"))
  expect_equal(unique(vapply(res, class, character(2))[1,]), c(class(sf_obj$x |> sf::st_union())[1],"sfc_POLYGON"))
  expect_true(sf::st_within(res$Buffers, res$All, sparse = FALSE))
  expect_true(sf::st_within(res$`0`, res$All, sparse = FALSE))
  expect_equal(sf::st_area(res$`10`)+sf::st_area(res$`20`)+sf::st_area(res$`30`), sf::st_area(res$Buffers), tolerance = 0.001)
  expect_equal(res[[1]], sf_obj |> sf::st_geometry() |> sf::st_union())
  expect_error(buffers(sf_obj, c(0, 10, 20, 30)), regexp = 'isTRUE(attr(dist, "generator") == "distances") is not TRUE', fixed = TRUE)
  expect_warning(buffers(sf_obj[integer(),], dist), regexp = "has no feature to compute geometries from", fixed = TRUE)

})
