#' @noRd
export <- function(x, file) {

  obj <- do.call(
    c,
    c(
      list(flight = x$flight),
      x$zones,
      x$segments |> lapply(\(x) {sf::st_geometry(x)|> sf::st_combine()})
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

  sf::st_write(obj, file, quiet = TRUE, delete_dsn = TRUE)

  return(invisible())

}
