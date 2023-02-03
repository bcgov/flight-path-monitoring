#' create template data.table for flight summary results
#'
#' @param tp sf object of track points layer of flight data
#' @param keep character vector of column to keep
#' @import sf
#' @return sf object
#' @export
#'
remove_contiguous_duplicates <- function(tp, keep) {

  # Less than 2 points, no need to drop any
  if (nrow(tp) < 2) {
    return(tp[,c(keep)])
  }
  # Extract coordinates
  coords <- tp |> st_coordinates()
  # Compare neighbouring points
  previous_seg_point_same_geom <- c(
    FALSE, # First point, no previous to compare to
    coords[-1L, 1L] == coords[-nrow(tp), 1L] & coords[-1L, 2L] == coords[-nrow(tp),2L]
  )
  # Remove contiguous duplicated geometries

  return(tp[which(!previous_seg_point_same_geom),keep])

}
