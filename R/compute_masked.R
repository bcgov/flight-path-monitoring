#' Compute masked points index
#' @import terra sf
#' @importFrom parallel mclapply detectCores
#' @return A logical vector.
#'
compute_masked <- function(pts, iz, dem, d) {

  # Transform points in the same CRS as Buffers
  # Use BC Albers because the units for the X,Y are the
  # same as the units for the elevation, a requirement
  # for `terra::viewshed`
  pts_crs <- pts |>
      sf::st_transform(sf::st_crs(iz[["Buffers"]]))

  # Project digital elevation model in the same crs
  dem <- dem |> terra::project(terra::crs(pts_crs))

  # In UWR polygon to SpatVector
  inuwrvect <- iz[["In UWR"]] |> terra::vect()

  # Get the cell numbers where the middle of the cell in within the SpatVector
  coi <- terra::cells(dem, inuwrvect)[,"cell"]

  # Boolean check
  vals <- seq_len(terra::ncell(dem)) %in% coi

  # Raster of cells of interest overlapping dem
  rcoi <- terra::rast(dem, vals = vals)

  # Base range adjustment for extent around point c(-min, +max)
  range <- do.call(max, d) |> as.numeric() * c(-1L, 1L)

  # Extract coordinates of pts in buffer and below max_altitude
  idx <- which(pts[["buffer"]] & !pts[["above_max_altitude"]])
  coords <- pts_crs[idx, ] |>
    sf::st_geometry() |>
    sf::st_coordinates()

  # Extract and convert above ground level
  agl <- pts_crs[idx, ][["agl"]] |> as.numeric()

  # This function checks if a flight point can see in a UWR zone within
  # the max buffer distance
  is_point_masked <- function(x) {

    # Create an extend to crop to from the point coordinates
    # Here we make the hypothesis that anything further than the max
    # buffer distance should not count in the viewshed analysis
    ext_cropped <- terra::ext(c(coords[x,"X"] + range, coords[x,"Y"] + range))

    # Crop area to reduce number of cells needed to compute viewshed
    dem_cropped <- terra::crop(dem, ext_cropped, snap = "out")
    rcoi_cropped <- terra::crop(rcoi, ext_cropped, snap = "out")

    # Viewshed
    v <- terra::viewshed(
      x = dem_cropped,
      loc = coords[x,],
      observer = agl[x],
      target = 1 # eyesight elevation of ungulates, guessing a meter?
    )

    # See if any cells of interest is visible from the flying object
    any(terra::extract(v, which(rcoi_cropped[][,1] == 1)))

  }

  # Compute for all points (in parallel on unix)
  res <- logical(nrow(pts))
  cores <- if (.Platform$OS.type == "unix") parallel::detectCores() - 1L else 1L
  res[idx] <- parallel::mclapply(
    X = seq_len(length(idx)),
    FUN = is_point_masked,
    mc.cores = cores
  ) |> unlist()

  return(res)

}

