#' Process flight data and produce summary table of time in zones
#'
#' @param flight Flight data obtained from `ingest_flights`
#' @param zones Geometries of incursion zones and telemetries.
#' @param dist A named numeric vector of distances from `distances`.
#' @param max_altitude Maximum altitude in meters for a track point to be considered.
#' @param geom_out A logical. If TRUE, some geometries used for the analysis are returned. Defaults to TRUE.
#' @param check_tiles Should the tiles that you already have in your cache be checked to see if
#' they need updating? Default FALSE. If you are running the same code frequently and are confident
#' the tiles haven't changed, setting this to FALSE will speed things up. For digital elevation
#' model from `bcmaps::cded`.
#' @import sf
#' @rawNamespace import(data.table, except=c(shift))
#' @return A list with a summary table of time in zones and optional geometries.
#' @export
#'
flight_process <- function(flight, zones, dist, max_altitude = 500, geom_out = TRUE, check_tiles = FALSE) {

  # Flight track points (contiguous duplicates combined)
  ftp <- flight[["track_points"]][, c("ele", "time", "track_seg_point_id")] |>
    remove_contiguous_duplicates()

  # Area of interest : Bounding box of flight points + Maximum incursion distance + 50m
  aoi <- ftp |>
    sf::st_bbox() |>
    sf::st_as_sfc() |>
    sf::st_buffer(max(dist) + 50L) |>
    sf::st_transform(crs = sf::st_crs(zones)) |>
    sf::st_bbox()

  # Zones of interest
  zoi <- filter_on_bbox(aoi, zones)

  if (nrow(zoi) == 0L) { return(empty_results(flight, dist)) } # No time to account for

  # Incursion
  incursions_zoi <- buffers(zoi, dist)

  # Points of interest
  poi <- compute_poi(ftp, incursions_zoi, aoi, max(dist), max_altitude, check_tiles = check_tiles)
  if (nrow(poi) == 0L) { return(empty_results(flight, dist)) } # No time to account for

  # Lines of interest
  loi <- compute_loi(poi, zoi)

  # Record filtered loi
  loi_filtered <- loi[which(loi[["filtered"]]),]
  # Keep loi with both endpoints not filtered
  loi <- loi[which(!loi[["filtered"]]),]
  if (nrow(loi) == 0L) { return(empty_results(flight, dist)) } # No time to account for

  # Compute time in each zone
  in_z <- time_in_zone(loi, incursions_zoi)

  res <- list("Summary" = do.call(
    data.table::data.table,
    c(
      list("Flight" = flight[["tracks"]][["name"]]),
      in_z[["Time in zones"]]
    )
  ))

  if (isTRUE(geom_out)) {
    res <- c(res, list(
      "Segments" = in_z[["Segments in zones"]],
      "Incursion zones" = incursions_zoi,
      "Filtered" = loi_filtered
    ))
  }

  return(res)

}

#' To reduce the size of the `process_flights` function
#' @noRd
#'
compute_poi <- function(ftp, iz, aoi, d, a, check_tiles = FALSE) {

  # Points of interest
  # Points that intersect with the zones and buffers + the points
  # before and after to create the LINESTRING at the zone entry

  # Transform points
  ftp_crs <- ftp |>
    sf::st_transform(crs = sf::st_crs(iz[["All"]]))

  # Points within area of interest + buffers
  pts_in_all <- ftp_crs |>
    sf::st_intersects(iz[["All"]], sparse = FALSE) |>
    which() # Extract index

  # Get the points in the buffers zone
  pts_in_buffer <- ftp_crs |>
    sf::st_intersects(iz[["Buffers"]], sparse = FALSE) |>
    which()

  # Add neighbor points, pmin/pmax to stay within valid points
  pts_to_keep <- c(pmax(0L, pts_in_all - 1L), pts_in_all, pmin(pts_in_all + 1L,nrow(ftp))) |>
    unique() |> sort() # Remove duplicates, sort indexes

  # Subset track points
  poi <- ftp[pts_to_keep,]
  if (nrow(poi) == 0L) { return(poi) } # No time to account for

  # Flag pts according to location
  poi[["outside"]] <- !pts_to_keep %in% pts_in_all
  poi[["buffer"]] <- pts_to_keep %in% pts_in_buffer

  # Check if points should be filtered because of terrain masking or altitude
  poi[["filtered"]] <- filtered(poi, iz[[1]], aoi, d, a, check_tiles = check_tiles)

  return(poi)

}

#' To reduce the size of the `process_flights` function
#' @noRd
#'
compute_loi <- function(poi, zoi) {

  # Transform points of interest to LINESTRING of interest
  # CRS transform is done after the union to avoid extra
  # computing cost of not using WSG 84 (s2 package)
  loi <- sf::st_union(
    sf::st_geometry(poi)[-nrow(poi)],
    sf::st_geometry(poi)[-1],
    by_feature = TRUE
  ) |>
    sf::st_cast("LINESTRING") |>
    sf::st_as_sf() |>
    sf::st_transform(crs = sf::st_crs(zoi))

  # Won't be using 3D distance as length calculation are for relative
  # ratio only, 3D would not have a large enough impact on hypothesis

  # Add line of interest metadata, above ground levels, time deltas
  # Keep point id for easier debugging, and in zone flag (to remove
  # LINESTRING between points outside the zones, the resulting straight
  # line could go through an incursion zone.
  loi[["track_seg_point_id_start"]] <- poi[["track_seg_point_id"]][-nrow(poi)]
  loi[["track_seg_point_id_end"]] <- poi[["track_seg_point_id"]][-1]

  loi[["filtered"]] <- poi[["filtered"]][-nrow(poi)] & poi[["filtered"]][-1]

  loi[["outside"]] <- poi[["outside"]][-nrow(poi)] & poi[["outside"]][-1]

  loi[["time_deltas"]] <- difftime(
    poi[["time"]][-1],
    poi[["time"]][-nrow(poi)],
    units = "secs"
  )

  # Keep line of interest with both endpoints not flagged as outside the zones
  loi <- loi[which(!loi[["outside"]]),]

  return(loi)

}


#' Create template data.table for flight summary results
#'
#' @param flight sf object of flight data
#' @param dist numeric distance
#' @noRd
#'
empty_results <- function(flight, dist) {
zero <- function(...) as.difftime(0, units = "secs")
do.call(
  data.table::data.table,
  args = c(
    list(
      "Flight" = flight[["tracks"]][["name"]]
    ),
    lapply(dist, zero),
    list(
      "All" = zero()
    )
  )
)
}
