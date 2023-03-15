#' Ingest flights data source
#'
#' @param dsn data source name. Either a character vector of file paths of a connection to a
#' a database. When a connection is used, it needs a query to retrieve flight track records.
#' @param query SQL query to select records.
#' @return A list of flights each represented by a list of spatial feature objects
#' @importFrom tools file_path_sans_ext
#' @export
#'
read_flights <- function(dsn, query = NULL) {

  # Character vector
  if (inherits(dsn, "character")) {

    dsn <- setNames(dsn, basename(dsn) |> tools::file_path_sans_ext())

    return(lapply(dsn, read_flight_file))

  } else if (inherits(dsn, "DBIConnection")) {

    stopifnot(!is.null(query))

    return(read_flight_db(dsn, query))

  }

}

#' Read a flight file
#'
#' @param fpath A file path.
#' @importFrom tools file_ext
#' @rdname read_flights
#' @export
#'
read_flight_file <- function(fpath) {

  if (!file.exists(fpath)) {
    warning("File does not exist : [", fpath, "]")
    return(NULL)
  }

  ext <- tools::file_ext(fpath) |> tolower()

  if (ext == "gpx") {

    # GPX file
    return(read_GPX(fpath))

  } else if (ext == "kml") {

    # KML file
    return(read_KML(fpath))

  } else {

    warning("File extension not supported : [", fpath, "] [ext: ", ext, "]")
    return(NULL)

  }

}

#' Read a GPX file into a list of simple feature objects
#'
#' @import sf
#' @rdname read_flights
#' @export
#'
read_GPX <- function(fpath) {

  # Extract layers metadata
  layers_data <- sf::st_layers(fpath)

  # Layers with at least 1 feature
  has_features <- which(layers_data[["features"]] > 0L)

  # Extract matching layer names
  non_empty_layers <-
    layers_data[["name"]][has_features] |>
    setNames(nm = _)

  # Extract each layers into an sf object
  return(
    lapply(
      X = non_empty_layers,
      FUN = sf::st_read,
      dsn = fpath,
      quiet = TRUE,
      drivers = "GPX"
    )
  )

}

#' Read a KML file into a list of simple feature object
#'
#' @import xml2
#' @rdname read_flights
#' @export
#'
read_KML <- function(fpath) {

  # Load file nodes
  nodes <- xml2::read_xml(fpath)

  # KML samples have two namespaces from `xml2::xml_ns(nodes)`
  # d1 <-> http://www.opengis.net/kml/2.2
  # gx <-> http://www.google.com/kml/ext/2.2

  # Process LineString which should be the flight path in CRS:4326
  tracks <- {

    # Find the LineString
    linestring_node <- nodes |> xml2::xml_find_first("//d1:LineString")

    name <- linestring_node |>
      xml2::xml_parent() |> # Move up
      xml2::xml_child("d1:name") |> # Get name node
      xml2::xml_text() # Extract text

    time <- linestring_node |>
      xml2::xml_parent() |> # Move up
      xml2::xml_child("d1:TimeStamp") |> # Get timestamp
      xml2::xml_child("d1:when") |> # Get when
      xml2::xml_text() |> # Extract text
      as.POSIXlt(tz = "UTC", format = "%Y-%m-%dT%H:%M:%OS") |> # Parse text to timestamp
      as.POSIXct() # Convert from POSIX list to compact format

    xyz_coords <- linestring_node |>
      xml2::xml_child("d1:coordinates") |> # Get LineString coordinates
      xml2::xml_text() |> # Extract text, it is just a big blob of text
      strsplit(" ", TRUE) |> # Split by space character to get individual "X,Y,Z"
      lapply(strsplit, split = ",") |> # Split by comma to get c("X","Y","Z")
      .subset2(1) |> # Extract first element, because of strsplit output format
      do.call(rbind, args = _) # Bind the rows together into matrix

    xyz_coords[,1:2] |> # Remove Z dimension
      apply(2, as.numeric) |> # Convert from character to numeric values
      list() |> # Insert into a list to respect `sf::multilinestring` requirements
      sf::st_multilinestring("XY") |> # Convert to multilinestring
      sf::st_geometry() |> # Extract geometry
      sf::st_sf(name = name, time = time, geometry = _) |> # Create an sf object with the geometry
      sf::st_set_crs(4326) # Set CRS to 4326
  }

  # Process gx:Track node which should be a collection
  # of XYZ track points with timestamps in CRS:4326
  # Assuming elevation is always in absolute mode
  # otherwise it would be messier to process as
  # we would need to know the Digital Elevation Model
  # used to derive the elevation values.
  track_points <- {

     # Find the gx:Track node
    track_node <- nodes |> xml2::xml_find_first("//gx:Track")

     # Extract timestamp of each gx::coord
    time <- track_node |>
      xml2::xml_find_all("d1:when") |> # Get the <when> nodes
      xml2::xml_text() |> # Get their text content
      as.POSIXlt(tz = "UTC", format = "%Y-%m-%dT%H:%M:%OS") |> # Parse text to timestamp
      as.POSIXct() # Convert from POSIX list to compact format

    # Create a simple feature from the coordinates
    track_node |>
      xml2::xml_find_all("gx:coord") |> # Find the <gx:coord> nodes
      xml2::xml_text() |> # Get their text content
      strsplit(split = " ") |> # Split by space character, split character is different than above
      do.call(rbind, args = _) |> # Bind the rows together into matrix
      apply(2, as.numeric) |> # Convert from character to numeric values
      data.table::as.data.table() |> # Convert to table structure
      data.table::setnames(c("x", "y", "ele")) |> # Set columns names
      data.table::set(j = "track_seg_point_id", value = seq_len(length(time))) |> # Add an id field
      data.table::set(j = "time", value = time) |> # Add a time field
      sf::st_as_sf(coords = c("x", "y"), crs = 4326) # Convert to sf object in CRS 4326

  }

  # Return in a format similar to read_GPX, so we can reuse
  # downstream in the flight processing pipeline
  return(
    list(
      tracks = tracks,
      track_points = track_points
    )
  )

}

#' Read flights from TRACKS database
#'
#' @importFrom DBI dbGetQuery
#' @rdname read_flights
#' @export
#'
read_flight_db <- function(dsn, query) {
  res <- DBI::dbGetQuery(dsn, query)
  # TODO: Process res to return similar objects than file methods
}
