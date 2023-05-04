#' Process flight data and produce summary table of time in zones
#'
#' @param flight Flight data obtained from `read_flight`.
#' @param zones Geometries of incursion zones and telemetries. Default to `default_zones()`.
#' @param dist A named numeric vector of distances from `distances`. Default to `default_dist()`.
#' @param max_altitude Maximum altitude in meters for a track point to be considered.
#' @param geom_out A logical. If TRUE, some geometries used for the analysis are returned.
#' Defaults to TRUE.
#' @param viewshed A logical. Perform viewshed analysis to remove flight segments masked by terrain.
#' Default to TRUE. Most compute intensive task of the analysis.
#' @param check_tiles Should the tiles that you already have in your cache be checked to see if
#' they need updating? Default FALSE. If you are running the same code frequently and are confident
#' the tiles haven't changed, setting this to FALSE will speed things up. For digital elevation
#' model from `bcmaps::cded`.
#' @import sf
#' @rawNamespace import(data.table, except=c(shift))
#' @return A list with a summary table of time in zones and optional geometries.
#' @export
#'
process_flight <- function(flight, zones = default_zones(), dist = default_dist(),
                           max_altitude = 500, geom_out = TRUE, viewshed = TRUE,
                           check_tiles = FALSE) {

  # Make sure flight comes from `read_flight`
  if (!inherits(flight, "character")) {
    stopifnot(isTRUE(attr(flight, "generator") == "read_flight"))
  }

  # Progress on Windows only
  p <- progressr::progressor(length(flight))

  # Process all flights and combine
  lapply(
    seq_len(length(flight)),
    \(x) {
      on.exit(p(sprintf("Flight analysed : [%s]", x)), add = TRUE)

      # Support only passing flight filepath as character to reduce memory usage for shinyapps.io
      if (inherits(flight[[x]], "character")) {
        flx <- read_flight(flight[[x]])
        names(flight)[x] <<- names(flx)
        flx <- flx[[1]]
      } else {
        flx <- flight[[x]]
      }

      process_one(
        flx,
        zones = zones,
        dist = dist,
        max_altitude = max_altitude,
        geom_out = geom_out,
        viewshed = viewshed,
        check_tiles = check_tiles,
        flight_id = x
      )
    }
  ) |>
    combine_res(flightname = names(flight), geom_out)

}

#' Process a single flight
#' @rdname process_flight
#' @param flight_id An integer. Flight id.
process_one <- function(flight, zones = default_zones(), dist = default_dist(),
                        max_altitude = 500, geom_out = TRUE, viewshed = TRUE,
                        check_tiles = FALSE, flight_id = 1L) {

  # Set default flight_id for processing a single flight
  flight[["tracks"]][["flight_id"]] <- flight_id

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

  # Create result object
  res <- empty_results(flight, dist)
  if (isTRUE(geom_out)) {
    res[["flight"]] <- flight[["tracks"]]
  }

  if (nrow(zoi) == 0L) { return(res) } # No time to account for

  # Add buffers
  zoi <- buffers(zoi, dist)

  # Points of interest
  poi <- compute_poi(
    ftp, zoi, aoi, max(dist), max_altitude,
    viewshed = viewshed, check_tiles = check_tiles
  )

  # Drop all and buffers
  zoi[c("all", "buffers")] <- NULL

  # Append zone of interest to result
  if (isTRUE(geom_out)) {
    res[["zones"]] <- zoi |>
      lapply(sf::st_transform, crs = sf::st_crs(flight[["tracks"]])) |>
      lapply(sf::st_as_sf) |>
      lapply(FUN = \(x) {
        x[["flight_id"]] <- flight_id
        return(x)
      })
  }

  if (nrow(poi) == 0L) { return(res) } # No time to account for

  # Lines of interest
  loi <- compute_loi(poi, sf::st_crs(zoi[[1]]), flight_id)

  # Record filtered loi
  if (isTRUE(geom_out)) {
    res[["segments"]][["filtered"]] <- loi[which(loi[["filtered"]]),] |>
      sf::st_transform(crs = sf::st_crs(flight[["tracks"]]))
  }

  # Keep loi with both endpoints not filtered
  loi <- loi[which(!loi[["filtered"]]),]
  if (nrow(loi) == 0L) { return(res) } # No time to account for

  # Compute time in each zone
  in_z <- time_in_zone(loi, zoi)

  # Map to existing result
  data.table::set(
    x = res[["summary"]],
    j = in_z[["time_in_zones"]] |> names(),
    value = in_z[["time_in_zones"]]
  )

  # Add segments when `geom_out` is true.
  if (isTRUE(geom_out)) {
    res[["segments"]] <- c(
      in_z[["segments_in_zones"]] |>
        lapply(sf::st_transform, crs = sf::st_crs(flight[["tracks"]])),
      res[["segments"]]
    )
  }

  return(res)

}

#' To reduce the size of the `process_flight` function
#' @noRd
#'
compute_poi <- function(ftp, iz, aoi, d, a, viewshed = TRUE, check_tiles = FALSE) {

  # Points of interest
  # Points that intersect with the zones and buffers + the points
  # before and after to create the LINESTRING at the zone entry

  # Transform points
  ftp_crs <- ftp |>
    sf::st_transform(crs = sf::st_crs(iz[["all"]]))

  # Points within area of interest + buffers
  pts_in_all <- ftp_crs |>
    sf::st_intersects(iz[["all"]], sparse = FALSE) |>
    which() # Extract index

  # Get the points in the buffers zone
  pts_in_buffer <- ftp_crs |>
    sf::st_intersects(iz[["buffers"]], sparse = FALSE) |>
    which()

  # Add neighbor points, pmin/pmax to stay within valid points
  pts_to_keep <- c(pmax(1L, pts_in_all - 1L), pts_in_all, pmin(pts_in_all + 1L,nrow(ftp))) |>
    unique() |> sort() # Remove duplicates, sort indexes

  # Subset track points
  poi <- ftp[pts_to_keep,]
  if (nrow(poi) == 0L) { return(poi) } # No time to account for

  # Flag pts according to location
  poi[["outside"]] <- !pts_to_keep %in% pts_in_all
  poi[["buffer"]] <- pts_to_keep %in% pts_in_buffer

  # Check if points should be filtered because of terrain masking or altitude
  poi[["filtered"]] <- filtered(
    poi, iz[[1]], aoi, d, a,
    viewshed = viewshed, check_tiles = check_tiles
  )

  return(poi)

}

#' To reduce the size of the `process_flight` function
#' @noRd
#'
compute_loi <- function(poi, crs, flight_id) {

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
    sf::st_transform(crs = crs)

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

  # Add flight_id
  loi[["flight_id"]] <- flight_id

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
  res <- list("summary" =
    do.call(
      data.table::data.table,
      args = c(
        list(
          "flight_id" = flight[["tracks"]][["flight_id"]],
          "name" = flight[["tracks"]][["name"]]
        ),
        lapply(dist, zero),
        list(
          "all" = zero()
        )
      )
    )
  )
  return(res)
}

#' Combine results of multiple process_one together
#' @noRd
combine_res <- function(res, flightname, geom_out) {

  # Combine list
  combined <- list()

  # Combine summary
  combined[["summary"]] <- res |>
    lapply(`[[`, "summary") |>
    data.table::rbindlist() |>
    data.table::set(j = "filename", value = flightname)

  # Combine others
  if (isTRUE(geom_out)) {

    # First level deep geometries
    nmlvl1 <- lapply(res, names) |> unlist() |> unique()

    for (nm in nmlvl1) {

      x <- lapply(res, `[[`, nm)

      if (nm == "flight") {

        combined[[nm]] <- do.call(rbind, args = x)

      } else if (!nm %in% "summary") {

        # Second level deep geometries
        nmlvl2 <- lapply(x, names) |> unlist() |> unique()

        for (nm2 in nmlvl2) {

          combined[[nm]][[nm2]] <- lapply(x, `[[`, nm2) |>
            do.call(rbind, args = _)

        }

      }

    }

  }

  attr(combined, "class") <- c("flightanalysis", class(combined))

  return(combined)

}
