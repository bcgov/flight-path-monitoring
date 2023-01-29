#' @importFrom terra rast viewshed
#' @importFrom sf st_is_longlat st_transform

#TODO determine correct crs EPSG:3005 ?

compute_viewshed <- function(dem, loc){

  dem <- rast(dem)

  #Convert long/lat to planar
  if (isTRUE(terra::is.lonlat(dem))){
    dem <- project(dem, "EPSG:3153")
  }

  if (isTRUE(sf::st_is_longlat(loc))){ #TODO handle case where loc has no crs
    loc <- sf::st_transform(loc,
                            crs = "+init=epsg:3153")
  }

  vs <- lapply(loc, \(x) terra::viewshed(x = dem2,
                        loc = x,
                        observer = 0,
                        target = 0))

  all_vs <- lapply(vs, values) |> as.data.frame() |> rowSums(na.rm=FALSE)|> as.logical()

  values(dem2) <- all_vs

  dem2
}

