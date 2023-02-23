test_that("Checks that append bbox add the four columns [xmin, ymin, xmax, ymax]", {

  test_obj <- sf::st_read(system.file("shape/nc.shp", package="sf"), quiet = TRUE)
  new_obj <- append_bbox_info(test_obj2 <- test_obj)

  expect_equal(setdiff(names(new_obj), names(test_obj)), c("xmin", "ymin", "xmax", "ymax"))
  expect_equal(as.data.frame(as.list(sf::st_bbox(new_obj[1,]))), sf::st_drop_geometry(new_obj)[1, c("xmin", "ymin", "xmax", "ymax")])

})
