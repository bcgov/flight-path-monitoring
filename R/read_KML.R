#' Read a KML file into a list of simple feature object
#'
#' @param file character string of filename
#' @return sf object
#' @import sf xml2 data.table
#' @export
#'
read_KML <- function(file) {

  # Checks
  stopifnot(
    file.exists(file), # Does the file exists
    {tools::file_ext(file) |> tolower()} == "kml" # Does it have a kml extension
  )

  # Load file nodes
  nodes <- xml2::read_xml(file)

  # KML samples have two namespaces from `xml2::xml_ns()`
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

    linestring_node |>
      xml2::xml_child("d1:coordinates") |> # Get LineString coordinates
      xml2::xml_text() |> # Extract text, it is just a big blob of text
      strsplit(" ", TRUE) |> # Split by space character to get individual "X,Y,Z"
      lapply(strsplit, split = ",") |> # Split by comma to get c("X","Y","Z")
      .subset2(1) |> # Extract first element, because of strsplit output format
      do.call(rbind, args = _) |> # Bind the rows together into matrix
      apply(2, as.numeric) |> # Convert from character to numeric values
      list() |> # Insert into a list to respect `sf::multilinestring` requirements
      sf::st_multilinestring("XYZ") |> # Convert to multilinestring
      sf::st_geometry() |> # Extract geometry
      sf::st_sf(name = name, time = time, geometry = _) |> # Create an sf object with the geometry
      sf::st_set_crs(4326) # Set CRS to 3426
  }

  # Process gx:Track node which should be a collection
  # of XYZ track points with timestamp in CRS:4326
  # assuming elevation is always in absolute mode
  # otherwise it would be messier to process as
  # we would need to know the Digital Elevation Model
  # used to derive the elevation values.
  track_points <- {

     # Find the gx:Track node
    track_node <- nodes |> xml2::xml_find_first("//gx:Track")

     # Extract timestamp of each gx::coord
    time <- track_node |>
      xml2::xml_find_all("d1:when") |> # Get the when nodes
      xml2::xml_text() |> # Get their text content
      as.POSIXlt(tz = "UTC", format = "%Y-%m-%dT%H:%M:%OS") |> # Parse text to timestamp
      as.POSIXct() # Convert from POSIX list to compact format

    # Create a simple feature from the coordinates
    track_node |>
      xml2::xml_find_all("gx:coord") |> # Find the gx:coord nodes
      xml2::xml_text() |> # Get their text content
      strsplit(split = " ") |> # Split by space character, split is different than above
      do.call(rbind, args = _) |> # Bind the rows together into matrix
      apply(2, as.numeric) |> # Convert from character to numeric values
      data.table::as.data.table() |> # Convert to table structure
      data.table::setnames(c("x", "y", "ele")) |> # Set columns names
      data.table::set(j = "track_seg_point_id", value = seq_len(length(time))) |> # Add an id field
      data.table::set(j = "time", value = time) |> # Add a time field
      sf::st_as_sf(coords = c("x", "y"), crs = 4326) # Convert to sf object

  }

  # Return in a format similar to read_GPX, so we can reuse
  # downstream in the flight processing pipeline
  return(list(tracks = tracks, track_points = track_points))

}
