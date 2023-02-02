#' filter sf object on an area of interest using bounding box information created by `append_bbox_info`
#'
#' @param flight sf object of flight data
#' @param dist numeric distance
#' @import data.table
#' @return data.table object
#' @export
#'
empty_results <- function(flight, dist) {
zero <- function(...) as.difftime(0, units = "secs")
do.call(
  data.table::data.table,
  args = c(
    list(
      "Flight" = flight[["tracks"]][["name"]],
      "In UWR" = zero()
    ),
    lapply(dist, zero),
    list(
      "All" = zero()
    )
  )
)
}
