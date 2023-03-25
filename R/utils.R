#' Print flight analysis
#' @param x Flight analysis results to print
#' @param ... further arguments passed to or from other methods.
#' @export
#'
print.flightanalysis <- function(x, ...) {
  print(x[["summary"]])
}

#' @noRd
winos <- function() {
  isTRUE(Sys.info()["sysname"] == "Windows")
}

#' @importFrom future plan multisession
#' @importFrom parallel mclapply detectCores
#' @importFrom future.apply future_lapply
#' @noRd
parlapply <- function() {
  if (!isTRUE(getOption("flight.path.monitoring.use.parallel", TRUE))) {
    lapply
  } else if (winos()) {
    options("future.rng.onMisuse" = "ignore")
    future::plan(future::multisession)
    future.apply::future_lapply
  } else {
    options("mc.cores" = parallel::detectCores())
    parallel::mclapply
  }
}

#' Print flight analysis
#' @param x object
#' @param y ignored
#' @param ... further specifications.
#' @param backend A character string. Either `leaflet` or `ggplot2`.
#' @param flight_id An integer. Which flight ids to plot. Default to all.
#' @param proxy For `leaflet` backend, a `leaflet::leafletProxy` object can be passed instead of initializing a new one.
#' @importFrom grDevices colorRampPalette
#' @importFrom leaflet addPolylines layersControlOptions addLayersControl addProviderTiles addPolygons
#' @importFrom ggplot2 geom_sf ggplot
#' @method plot flightanalysis
#' @export
#'
plot.flightanalysis <- function(x, y, ..., backend = c("leaflet", "ggplot2"), flight_id = NULL, proxy = NULL) {

  backend <- match.arg(backend)

  # Check if there are geometries
  if (is.null(x[["flight"]])) {
    warning("No geometries were found of the object.\nRerun `process_flight` with `geom_out` set to `TRUE`.")
    return()
  }

  # Filter by flight id
  if (!is.null(flight_id)) {
    filt <- function(data) {
      data[data[["flight_id"]] %in% flight_id,]
    }
  } else {
    filt <- identity
  }

  if (backend == "leaflet") {

    if (is.null(proxy)) {
      p <- leaflet::leaflet() |>
        leaflet::addProviderTiles(provider = "Esri.WorldTopoMap")
    } else {
      p <- proxy
    }

    overlayGroups <- character()

    # Add zones
    if (lenzon <- length(x[["zones"]])) {
      fillOpacity <- 0.225 / lenzon + 0.125
      for (z in x[["zones"]]) {
        p <- p |>
          leaflet::addPolygons(
            data = filt(z),
            color = "white",
            opacity = 1,
            weight = 1,
            fillColor = "#db0f27",
            fillOpacity = fillOpacity,
            group = "Zones"
          )
        fillOpacity <- fillOpacity - 0.225 / lenzon
      }
      overlayGroups <- c(overlayGroups, "Zones")
    }

    # Add flight
    p <- p |>
      leaflet::addPolylines(
        data = filt(x[["flight"]]),
        weight = 1,
        color = "darkgreen",
        dashArray = 4,
        group = "Flight Tracks"
      )
    overlayGroups <- c("Flight Tracks", overlayGroups)

    # Add segments
    if (lenseg <- length(x[["segments"]])) {
      cf <- grDevices::colorRampPalette(c("darkblue","skyblue"))
      colors <- cf(lenseg-1)
      for (s in x[["segments"]]) {
        p <- p |>
          leaflet::addPolylines(
            data = filt(s),
            weight = 2,
            color = colors[1],
            opacity = 1,
            group = "Segments"
          )
        colors <- colors[-1]
        if (!length(colors)) colors <- "deeppink"
      }
      overlayGroups <- c(overlayGroups, "Segments")
    }

    # Add controls
    p <- p |>
      leaflet::addLayersControl(
        overlayGroups = overlayGroups,
        options = leaflet::layersControlOptions(collapsed = FALSE)
      )

  } else {

    p <- ggplot2::ggplot()

    # Add zones
    if (lenzon <- length(x[["zones"]])) {
      cf <- grDevices::colorRampPalette(c("red", "yellow"))
      colors <- cf(lenzon)
      for (z in x[["zones"]]) {
        p <- p +
          ggplot2::geom_sf(
            data = filt(z) |> sf::st_geometry(),
            fill = colors[1]
          )
        colors <- colors[-1]
      }
    }

    # Add flight
    p <- p +
      ggplot2::geom_sf(
        data = x[["flight"]] |> filt() |> sf::st_geometry(),
        colour = "lightgreen"
      )

    # Add segments
    if (lenseg <- length(x[["segments"]])) {
      cf <- grDevices::colorRampPalette(c("darkblue","skyblue"))
      colors <- cf(lenseg-1)
      for (s in x[["segments"]]) {
        p <- p +
          ggplot2::geom_sf(
            data = s |> filt() |> sf::st_geometry(),
            colour = colors[1]
          )
        colors <- colors[-1]
        if (!length(colors)) colors <- "deeppink"
      }
    }

  }

  return(p)

}
