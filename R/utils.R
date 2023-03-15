#' @noRd
cores <- function() {
  cores <- if (.Platform$OS.type == "unix") parallel::detectCores() - 1L else 1L
  cores <- getOption("flight.path.monitoring.maxcores", cores)
  return(cores)
}

#' Print flight analysis
#' @param x Flight analysis results to print
#' @param ... further arguments passed to or from other methods.
#' @export
#'
print.flightsummary <- function(x, ...) {
  print(x[["summary"]])
}

#
# #' Print flight analysis
# #' @param x object
# #' @param y ignored
# #' @param ... further specifications.
# #' @param backend A character string. Either `leaflet` or `ggplot2`.
# #' @export
# #'
# plot.flightsummary <- function(x, y, ..., backend = c("leaflet", "ggplot2")) {
#
# }
