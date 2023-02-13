#' read a gpx file into an r object
#'
#' @param file character string of filename
#' @return sf object
#' @import sf
#' @export
#'
read_GPX <- function(file) {
  layers_data <- sf::st_layers(file)
  non_empty <- layers_data[["name"]][which(layers_data[["features"]] > 0)] |>
    setNames(nm = _)
  lapply(non_empty, st_read, dsn = file, quiet = TRUE)
}
