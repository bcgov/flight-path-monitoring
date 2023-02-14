#' process flight data to produce summary table of segments in zones of interest
#'
#' @param flight flight data obtained using `read_GPX`
#' @param zones geometries of incursion zones
#' @param telemetry geometries of points of interest using telemetry information
#' @param dist list named list of distance in meters defined using `set_units`
#' @param max_altitude distance in meters defined using `set_units` to represent the maximum altitude for a track point to be considered
#' @param segments boolean, if TRUE segments within zones of interest are returned. Defaults to TRUE
#' @importFrom bcmaps cded
#' @importFrom units set_units
#' @import terra
#' @import sf
#' @import data.table
#' @return list summary table of time and segments in incursion zones
#' @export
#'
flight_process <- function(flight, zones, telemetry, dist, max_altitude = units::set_units(500, m), segments = TRUE) {

  # Flight track points (contiguous duplicates combined)
  ftp <- flight[["track_points"]] |>
    remove_contiguous_duplicates(keep = c("ele", "time", "track_seg_point_id"))

  # Area of interest : Bounding box of flight points + Maximum incursion distance + 50m
  aoi <- ftp |>
    sf::st_bbox() |>
    sf::st_as_sfc() |>
    sf::st_buffer(do.call(max,dist) + units::set_units(50L, m)) |>
    sf::st_transform(crs = sf::st_crs(zones)) |>
    sf::st_bbox()

  # Zones of interest
  zoi <- filter_on_bbox(aoi, zones)

  # Telemetry of interest
  #TODO: Consider data time for telemetry?
  # toi <- filter_on_bbox(aoi, telemetry)
  # buffer than merge with zoi

  if (nrow(zoi) == 0L) { return(empty_results(flight, dist)) } # No time to account for

  # Incursion
  incursions_zoi <- compute_donut(dist, zoi)

  # Points of interest
  poi <- compute_poi(ftp, incursions_zoi, aoi, dist, max_altitude)
  if (nrow(poi) == 0L) { return(empty_results(flight, dist)) } # No time to account for

  # Lines of interest
  loi <- compute_loi(poi, zoi)

  # Record filtered loi
  loi_filtered <- loi[which(loi[["filtered"]]),]
  # Keep loi with both endpoints not filtered
  loi <- loi[which(!loi[["filtered"]]),]
  if (nrow(loi) == 0L) { return(empty_results(flight, dist)) } # No time to account for

  # Compute time in each zone
  # TODO: Can it be optimize? Could we avoid computing for line of interest
  # Completly in UWR zone (flagged as inside)
  in_z <- compute_in_zone(loi, incursions_zoi)

  res <- list("Summary" = do.call(
    data.table::data.table,
    c(
      list("Flight" = flight[["tracks"]][["name"]]),
      in_z[["Time in zones"]]
    )
  ))

  if (isTRUE(segments)) {
    res <- c(res, list(
      "Segments" = in_z[["Segments in zones"]],
      "Incursion zones" = incursions_zoi,
      "Filtered" = loi_filtered
    ))
  }

  return(res)

}
