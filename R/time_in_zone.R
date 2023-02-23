#' Compute time spent in zones
#'
#' @param sf_obj A spatial feature object with `LINESTRING` geometries and field `time_deltas`.
#' @param zones A named list of `POLYGON` geometries.
#' @param exclude Character vector of zone names to exclude.
#' @import sf
#' @return A list with `LINESTRING` segments and time spent in each zone.
#' @details Also return an `All`
#' element which is the sum of of time spent in each zones. It is not the time spent in a union
#' of all zones geometries. If the zones overlap, this information would be important to
#' communicate to avoid confusion.
#' @export
#'
time_in_zone <- function(sf_obj, zones, exclude = c("Buffers", "All")) {

  stopifnot(
    inherits(sf_obj, "sf"),
    inherits(sf_obj |> sf::st_geometry(), "sfc_LINESTRING"),
    all(vapply(zones, inherits, logical(1), c("sfc_MULTIPOLYGON", "sfc_POLYGON"))),
    !is.null(names(zones)),
    "time_deltas" %in% names(sf_obj)
  )

  # Compute full length of LINESTRING
  sf_obj[["full_length"]] <- sf::st_length(sf_obj)

  # Set agr to avoid warning on intersection, since fields are kept constant with each feature
  sf::st_agr(sf_obj) <- factor("constant", levels(sf::st_agr(sf_obj)))

  # Compute intersection of LINESTRING geometries and zones
  inter <- parallel::mclapply(
    mc.cores = cores(),
    zones[!names(zones) %in% exclude],
    function(z) {
      sf::st_intersection(sf_obj, z)
    }
  )

  # Compute time spent in zone as the ratio of LINESTRING length in the zone to the original
  # LINESTRING length multiplied by the time difference recorded between the two endpoints forming
  # the LINESTRING.
  time_in_z <- lapply(inter, function(z) {
    sum(
      na.rm = TRUE,
      ((sf::st_length(z) / z[["full_length"]]) |> as.numeric()) * z[["time_deltas"]]
    )
  })

  # Add an `All` element summing over all zones
  time_in_z[["All"]] <- do.call(sum, time_in_z)

  return(
    list(
      "Segments in zones" = inter,
      "Time in zones" = time_in_z
    )
  )

}
