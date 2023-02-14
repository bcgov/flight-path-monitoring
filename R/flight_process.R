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

  # Digital elevation model
  dem <- bcmaps::cded(aoi, check_tiles = FALSE) |> terra::rast()

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
  # Points that intersect with the zones and buffers + the points
  # before and after to create the LINESTRING at the zone entry

  # Transform points
  ftp_crs <- ftp |>
    sf::st_transform(crs = sf::st_crs(incursions_zoi[["All"]]))

  # Points within area of interest + buffers
  pts_in_all <- ftp_crs |>
    sf::st_intersects(incursions_zoi[["All"]], sparse = FALSE) |>
    which() # Extract index

  # Get the points in the buffers zone
  pts_in_buffer <- ftp_crs |>
    sf::st_intersects(incursions_zoi[["Buffers"]], sparse = FALSE) |>
    which()

  # Get the points in the uwr zone
  pts_in_uwr <- ftp_crs |>
    sf::st_intersects(incursions_zoi[["In UWR"]], sparse = FALSE) |>
    which() # Extract index

  # Add neighbor points, pmin/pmax to stay within valid points
  pts_to_keep <- c(pmax(0L, pts_in_all - 1L), pts_in_all, pmin(pts_in_all + 1L,nrow(ftp))) |>
    unique() |> sort() # Remove duplicates, sort indexes

  # Subset track points
  poi <- ftp[pts_to_keep,]
  if (nrow(poi) == 0L) { return(empty_results(flight, dist)) } # No time to account for

  # Get ground level of points of interest
  poi[["ground_ele"]] <-
    terra::extract(
      x = dem,
      y = poi |>
        sf::st_transform(crs = sf::st_crs(dem)) |>
        terra::vect(),
      method = "bilinear",
      ID = FALSE
    )[,1]

  # Above ground level (sometimes source elevation is wrong)
  # This is where we might want to do some smoothing if flight ele is bad
  # Currently do same as python, limit to 0L
  poi[["agl"]] <- pmax(poi[["ele"]] - poi[["ground_ele"]], 0) |> units::set_units(m)
  poi[["above_max_altitude"]] <- poi[["agl"]] > max_altitude

  # Flag pts according to location
  poi[["outside"]] <- !pts_to_keep %in% pts_in_all
  poi[["inside"]] <- pts_to_keep %in% pts_in_uwr
  poi[["buffer"]] <- pts_to_keep %in% pts_in_buffer

  # Check if points are masked by terrain
  poi[["masked_by_terrain"]] <- compute_masked(poi, incursions_zoi, dem, dist)

  # Flag filtered points
  poi[["filtered"]] <- poi[["above_max_altitude"]] | poi[["masked_by_terrain"]]

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
  loi[["inside"]] <- poi[["inside"]][-nrow(poi)] & poi[["inside"]][-1]
  loi[["time_deltas"]] <- difftime(
    poi[["time"]][-1],
    poi[["time"]][-nrow(poi)],
    units = "secs"
  )

  # Keep line of interest with both points not flagged as outside the zones
  loi <- loi[which(!loi[["outside"]]),]

  # Record filtered loi
  loi_filtered <- loi[which(loi[["filtered"]]),]
  # Keep loi with both points not filtered
  loi <- loi[which(!loi[["filtered"]]),]

  if (nrow(loi) == 0L) { return(empty_results(flight, dist)) } # No time to account for

  # Compute original length of linestring before apply incursion zones / viewshed masks
  loi[["unmasked_length"]] <- sf::st_length(loi)

  # Set agr to avoid warning on intersection, since fields are kept constant with each feature
  sf::st_agr(loi) <- factor("constant", levels(sf::st_agr(loi)))

  # Compute time in each zone
  # TODO: Can it be optimize? Could we avoid computing for line of interest
  # Completly in zone
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
