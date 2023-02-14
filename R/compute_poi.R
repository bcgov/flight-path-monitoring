compute_poi <- function(ftp, iz, aoi, d, max_altitude) {

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

  # Digital elevation model
  dem <- bcmaps::cded(aoi, check_tiles = FALSE) |> terra::rast()

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
  poi[["buffer"]] <- pts_to_keep %in% pts_in_buffer

  # Check if points are masked by terrain
  poi[["masked_by_terrain"]] <- compute_masked(poi, iz, dem, d)

  # Flag filtered points
  poi[["filtered"]] <- poi[["above_max_altitude"]] | poi[["masked_by_terrain"]]

  return(poi)

}
