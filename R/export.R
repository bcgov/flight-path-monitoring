#' Export analysis results
#' @param x The result object to be written.
#' @param file A filename using kml extension.
#' @param flight_id Flight ID for filtering.
#' @export
#' @importFrom tools toTitleCase
export <- function(x, file, flight_id = NULL) {

  filter <- function(x) {
    if (is.null(flight_id)) {
      return(x)
    } else {
      if (inherits(x, "list")) {
        return(lapply(x, \(x) {x[x[["flight_id"]] %in% flight_id,]}))
      } else {
        return(x[x[["flight_id"]] %in% flight_id,])
      }
    }
  }

  obj <- do.call(
    c,
    c(
      list(flight = filter(x$flight)) |> lapply(\(x) {sf::st_geometry(x)|> sf::st_combine()}),
      filter(x$zones) |> lapply(\(x) {sf::st_geometry(x)|> sf::st_combine()}),
      filter(x$segments) |> lapply(\(x) {sf::st_geometry(x)|> sf::st_combine()})
    ) |>
      lapply(sf::st_geometry)
  ) |> sf::st_as_sf()

  obj[["name"]] <- c(
    "Flight Path",
    paste0(
      "Buffer Zone - ",
      names(x$zones) |> gsub("_", " ", x = _) |> tools::toTitleCase()
    ),
    paste0(
      "Flight Segment - ",
      names(x$segments) |> gsub("_", " ", x = _) |> tools::toTitleCase()
    )
  )

  return(sf::st_write(obj, file, quiet = TRUE, delete_dsn = ifelse(file.exists(file), TRUE, FALSE)))

}

style_kml <- function(file) {

}
