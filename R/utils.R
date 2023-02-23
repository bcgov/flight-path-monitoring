#' @noRd
cores <- function() {
  cores <- if (.Platform$OS.type == "unix") parallel::detectCores() - 1L else 1L
  cores <- getOption("flight.path.monitoring.maxcores", cores)
  return(cores)
}
