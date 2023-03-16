#' Compute masked points index
#' @param sf_obj A spatial feature object of flight track points.
#' @param zone A geometry of type `sfc_POLYGON` representing the reference zone without buffers.
#' @param aoi Area of interest used to extract the right digital elevation tiles.
#' @param max_dist Maximum distance below which a track point can be considered.
#' @param max_altitude distance in meters to represent the maximum altitude for a track point to be considered.
#' @param check_tiles Should the tiles that you already have in your cache be checked to see if
#' they need updating? Default FALSE. If you are running the same code frequently and are confident
#' the tiles haven't changed, setting this to FALSE will speed things up. For digital elevation
#' model from `bcmaps::cded`.
#' @import terra sf
#' @importFrom bcmaps cded
#' @return A logical vector.
#'
filtered <- function(sf_obj, zone, aoi, max_dist, max_altitude, check_tiles = FALSE) {
# 80% of the processing is spent in this function. viewshed is quite compute intensive + dem

  # Digital elevation model
  dem <- bcmaps::cded(aoi, check_tiles = check_tiles) |> terra::rast()

  # Transform `sf_obj` points in the same CRS as Buffers
  # Use BC Albers because the units for the X,Y are the
  # same as the units for the elevation, a requirement
  # for `terra::viewshed`
  sf_obj_in_crs <- sf_obj |> sf::st_transform(sf::st_crs(zone))

  # Get ground level of points of interest in `sf_obj`
  ground_ele <-
    terra::extract(
      x = dem,
      y = sf_obj |>
        sf::st_transform(crs = sf::st_crs(dem)) |>
        terra::vect(),
      method = "bilinear",
      ID = FALSE
    )[,1]

  # Above ground level (sometimes source elevation is wrong)
  # This is where we might want to do some smoothing if flight ele is bad
  # Currently do same as python, limit to 0L
  agl <- pmax(sf_obj[["ele"]] - ground_ele, 0)
  above_max_altitude <- agl > max_altitude

  # Extract coordinates of `sf_obj` located in buffer zones and below max_altitude
  idx <- which(sf_obj[["buffer"]] & !above_max_altitude)
  coords <- sf_obj_in_crs[idx, ] |>
    sf::st_geometry() |>
    sf::st_coordinates()

  # Extract and convert above ground level
  agl <- agl[idx]

  # Project digital elevation model in the same crs
  dem <- dem |> terra::project(terra::crs(sf_obj_in_crs))

  # Reference polygon shape (without buffers) to SpatVector
  refvect <- zone |> terra::vect()

  # Get the cell numbers where the middle of the cell is within the SpatVector
  refcells <- terra::cells(dem, refvect)[,"cell"]

  # Turn it into a logical vector
  refvals <- seq_len(terra::ncell(dem)) %in% refcells

  # Raster of polygon cells; TRUE when they overlap dem
  # Both raster, dem and this one will share the same extent
  refrast <- terra::rast(dem, vals = refvals)

  # Reference range around coordinates used for `crop` with masking.
  # Here we make the hypothesis that anything further than the max
  # buffer distance should not count in the viewshed analysis
  refrange <- coords |> terra::vect() |> terra::buffer(max_dist)

  # Wrap on Windows
  if (winos()) {
    dem <- dem |> terra::wrap()
    refrange <- refrange |> terra::wrap()
    refrast <- refrast |> terra::wrap()
    unwrapper <- function() {
      if (inherits(dem, "PackedSpatRaster")) { dem <<- terra::unwrap(dem)}
      if (inherits(refrast, "PackedSpatRaster")) { refrast <<- terra::unwrap(refrast)}
      if (inherits(refrange, "PackedSpatVector")) { refrange <<- terra::unwrap(refrange)}
    }
  } else {
    unwrapper <- function() {}
  }

  # This function checks if a flight point can see in a reference zone
  # within the max distance
  is_point_masked <- function(x) {

    unwrapper()

    # Crop area to reduce number of cells needed to compute viewshed
    dem_cropped <- terra::crop(dem, refrange[x], mask = TRUE, snap = "out")
    refrast_cropped <- terra::crop(refrast, refrange[x], mask = TRUE, snap = "out")

    # Viewshed
    v <- terra::viewshed(
      x = dem_cropped,
      loc = coords[x,],
      observer = agl[x],
      target = 1 # eyesight elevation of ungulates, guessing a meter?
    )

    # See if any reference zone cells is terrain masked from the coordinates
    # of the observer within the max distance radius.
    !any(terra::extract(v, which(refrast_cropped[][,1] == 1)))

  }

  # Compute for all points
  terrain_masked <- logical(nrow(sf_obj))
  terrain_masked[idx] <- parlapply()(
    X = seq_len(length(idx)),
    FUN = is_point_masked
  ) |> unlist()

  res <- terrain_masked | above_max_altitude

  return(res)

}

