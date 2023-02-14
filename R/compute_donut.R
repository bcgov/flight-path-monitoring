#' Create a buffer geometry of distance `dist` around existing geometries
#'
#' @param dist distance of buffer
#' @param sf_obj sf object
#' @return list
#' @export
#'
compute_donut <- function(dist, sf_obj) {

  buff_oven <- function() {
    precomputed_dist <- list(
      "0" = sf_obj |> sf::st_geometry() |> sf::st_union()
    )
    function(d) {
      d_char <- as.character(d)
      if (is.null(res <- precomputed_dist[[d_char]])) {
        precomputed_dist[[d_char]] <<- res <-
          sf::st_geometry(sf_obj) |> sf::st_buffer(d) |> sf::st_union()
      }
      return(res)
    }
  }

  buffers <- buff_oven()

  bake_donut <- function(d) {
    d <- sort(d)
    sf::st_difference(buffers(d[2]), buffers(d[1]))
  }

  if (nrow(sf_obj)) {
    return(
      c(
        list("In UWR" = buffers("0")),
        lapply(dist, bake_donut),
        {
          allzones <- buffers(do.call(max, dist) |> as.character())
          allbuffers <- sf::st_difference(allzones, buffers("0"))
          list(
            "Buffers" = allbuffers,
            "All" = allzones
          )
        }
      )
    )
  }

  return(invisible())
}
