#' Read a GPX file into a list of simple feature object
#'
#' @param file A character. Path to a GPX file.
#' @return sf object
#' @import sf
#' @export
#'
read_GPX <- function(file) {

  # Checks
  stopifnot(
    file.exists(file), # Does the file exists
    {tools::file_ext(file) |> tolower()} == "gpx" # Does it have a gpx extension
  )

  # Extract layers metadata
  layers_data <- sf::st_layers(file)

  # Layers with at least 1 feature
  has_features <- which(layers_data[["features"]] > 0)

  # Extract matching layer names
  non_empty <-
    layers_data[["name"]][has_features] |>
    setNames(nm = _)

  # Extract each layers into an sf object
  lapply(
    X = non_empty,
    FUN = st_read,
    dsn = file,
    quiet = TRUE,
    drivers = "GPX"
  )

}
