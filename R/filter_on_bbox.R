#' filter sf object on an area of interest using bounding box information created by `append_bbox_info`
#'
#' @param aoi geometry of area of interest
#' @param sf_obj sf object
#' @return sf object filtered on area of interest
#' @export
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
