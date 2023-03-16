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

#
# #' Print flight analysis
# #' @param x object
# #' @param y ignored
# #' @param ... further specifications.
# #' @param backend A character string. Either `leaflet` or `ggplot2`.
# #' @param flight_id An integer. Which flight ids to plot. Default to all.
# #' @export
# #'
# plot.flightanalysis <- function(x, y, ..., backend = c("leaflet", "ggplot2"), flight_id = NULL) {
#
# }
