#' Remove contiguous points in flight path that have the same XY, Z not taken into account.
#' This will reduce the amount of point processing needed and yield the same results.
#' @noRd
#' @param sf_obj Track points spatial feature object
#' @import sf
#'
remove_contiguous_duplicates <- function(sf_obj) {

  stopifnot(
    inherits(sf_obj, "sf"),
    inherits(sf_obj |> sf::st_geometry(), "sfc_POINT")
  )

  # Less than 2 points, no need to drop any
  if (nrow(sf_obj) < 2) {
    return(sf_obj)
  }
  # Extract coordinates
  coords <- sf_obj |> sf::st_coordinates()
  # Compare neighbouring points
  previous_seg_point_same_geom <- c(
    FALSE, # First point, no previous point to compare to
    # Logical condition
    # X coordinate at position x = X coordinate at position x-1 (from x = 2 to x = length(X)) AND
    # Y coordinate at position x = Y coordinate at position x-1 (from x = 2 to x = length(X))
    #
    # We use the negative element notation to drop element from the vector, essentially shifting
    # the position of the coordinates so they can be compared.
    coords[-1L, 1L] == coords[-nrow(sf_obj), 1L] & coords[-1L, 2L] == coords[-nrow(sf_obj),2L]
  )

  # Remove contiguous duplicated geometries
  return(sf_obj[which(!previous_seg_point_same_geom),])

}
