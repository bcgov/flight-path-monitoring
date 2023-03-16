#' Create a bounding box for each geometries in an sf object
#'
#' @param sf_obj sf object
#' @return same sf object with 4 new columns : xmin, ymin, xmax and ymax
#' @import sf
#' @details Used for faster lookup when selecting area of interest.
#'
append_bbox_info <- function(sf_obj) {

  stopifnot(inherits(sf_obj, "sf"))

  # Switch class, store bbox for faster processing later
  geom <- sf::st_geometry(sf_obj)

  cls_ori <- class(sf_obj)

  # Switch to data.table class
  data.table::setDT(sf_obj)

  data.table::set(
    sf_obj,
    j = c("xmin", "ymin", "xmax", "ymax"),
    value = as.list(
      data.table::rbindlist(
        lapply(
          geom,
          function(x) {
            as.list(
              # Extract bounding box for each geometries
              sf::st_bbox(x)
            )
          }
        )
      )
    )
  )

  # And switch back into original class
  attr(sf_obj, "class") <- cls_ori

  return(sf_obj)

}

#' filter sf object on an area of interest using bounding box information created by `append_bbox_info`
#'
#' @param aoi geometry of area of interest
#' @return sf object filtered on area of interest
#' @rdname append_bbox_info
#'
filter_on_bbox <- function(aoi, sf_obj) {
  sf_obj[
    which(
      .subset2(aoi, "xmin") < .subset2(sf_obj, "xmax") &
        .subset2(aoi, "xmax") > .subset2(sf_obj, "xmin") &
        .subset2(aoi, "ymin") < .subset2(sf_obj, "ymax") &
        .subset2(aoi, "ymax") > .subset2(sf_obj, "ymin")
    ),
  ]
}
