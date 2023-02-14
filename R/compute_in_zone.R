#' compute time in incursion zones and creates a summary table
#'
#' @param loi geometry linestring of interest
#' @param incz geometries of incursion zones
#' @import sf
#' @import parallel
#' @return list summary table of time and segments in incursion zones
#' @export
#'
compute_in_zone <- function(loi, incz) {

  cores <- if (isTRUE(.Platform$OS.type == "unix")) parallel::detectCores() - 1L else 1L

  inter <- parallel::mclapply(
    mc.cores = cores,
    incz[!names(incz) %in% c("Buffers", "All")],
    function(z) {
      sf::st_intersection(loi, z)
    }
  )

  time_in_z <- lapply(inter, function(z) {
    sum(
      na.rm = TRUE,
      ((sf::st_length(z) / z[["unmasked_length"]]) |> as.numeric()) * z[["time_deltas"]]
    )
  })

  time_in_z[["All"]] <- do.call(sum, time_in_z)

  return(list("Segments in zones" = inter, "Time in zones" = time_in_z))

}
