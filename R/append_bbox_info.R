#' Create a bounding box for each geometries in an sf object
#'
#' @param sf_obj sf object
#' @return sf object with 4 new columns : xmin, ymin, xmax and ymax
#' @import sf
#' @import data.table
#' @export
#'
append_bbox_info <- function(sf_obj) {
  stopifnot(inherits(sf_obj, "sf"))
  # Switch class, store bbox for faster processing later
  geom <- st_geometry(sf_obj)
  cls_ori <- class(sf_obj)
  setDT(sf_obj)
  set(
    sf_obj,
    j = c("xmin", "ymin", "xmax", "ymax"),
    value = as.list(data.table::rbindlist(lapply(geom, function(x) as.list(st_bbox(x)))))
  )
  attr(sf_obj, "class") <- cls_ori
  return(sf_obj)
}
