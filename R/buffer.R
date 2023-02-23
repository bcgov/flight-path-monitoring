#' Define buffer distances
#'
#' @param ... Named buffer distances. If unnamed, will use the distance as names.
#' @param reflabel A character string. Reference geometries name from which the
#' buffers are computed. Default to `In UWR`.
#' @examples
#' distances(High = 1500, Moderate = 1000, Low = 500, reflabel = "In UWR")
#' @return A sorted named integer vectors.
#' @importFrom stats setNames
#' @export
distances <- function(..., reflabel = "In UWR") {

  dist <- list(...) |> unlist()

  # Check if all distances are greater than 0 and
  # all distances are different
  stopifnot(
    all(dist > 0L),
    all(!duplicated(dist)),
    all(!names(dist) %in% c("All", "Buffers")) # Names reserved for other purpose
  )

  # Set names if not provided
  if (names(dist) |> is.null()) {
    names(dist) <- dist |> as.character()
  } else if (any(empty <- nchar(names(dist)) == 0L)) {
    names(dist)[which(empty)] <- dist[which(empty)] |> as.character()
  }

  # Add a refzone and sort
  refzone <- setNames(0L, reflabel)
  dist <- c(refzone, dist) |> sort()

  # Set an attribute to flag the distances were processed by this function
  attr(dist, "generator") <- "distances"

  dist

}

#' Compute buffers around geometries
#'
#' @param dist A named integer vector. buffer distances as produced by `distances`.
#' @param sf_obj A spatial feature object from which geometries can be extracted.
#' @return A list of named buffers as spatial feature objects. Names recycled from `dist`.
#' @export
#'
buffers <- function(sf_obj, dist) {

  # Make sure distances come from `distances`
  stopifnot(isTRUE(attr(dist, "generator") == "distances"))

  # Cache buffers as they are computed to allow result recycling
  # Definition part
  buffer_cache <- function() {
    # This list will hold the cached buffer in its own environment
    # Initialized with the union of the reference geometries
    cached <- lapply(
      dist[1],
      \(x) {sf_obj |> sf::st_geometry() |> sf::st_union()}
    )
    # This will return a function that can access the list above, check if
    # a buffer has been computed or compute it as needed.
    function(nm) {
      if (is.null(res <- cached[[nm]])) {
        cached[[nm]] <<- res <-
          sf_obj |> sf::st_geometry() |> sf::st_buffer(dist[[nm]]) |> sf::st_union()
      }
      return(res)
    }
  }

  # Usage part of the above definition
  cache <- buffer_cache()

  # Compute the difference between two buffers, resulting in a ring like shape
  buffer_ring <- function(d1, d2) {
    sf::st_difference(cache(d1), cache(d2))
  }

  if (nrow(sf_obj)) {

    # Return a list of buffer geometries with two additional `Buffers` and `All`. `Buffers` is
    # all the buffers combined, `All` is all the buffers and the reference geometries.
    res <-
      c(
        lapply(dist[1], \(x) {cache(names(dist)[1])}),
        mapply(buffer_ring, names(dist)[-1], names(dist)[-length(dist)], SIMPLIFY = FALSE),
        list(
          "Buffers" = buffer_ring(names(dist)[length(dist)], names(dist)[1]),
          "All" = cache(names(dist)[length(dist)])
        )
      )
    return(res)
  } else {
    warning("`sf_obj` has no feature to compute geometries from.")
    return(invisible())
  }

}
