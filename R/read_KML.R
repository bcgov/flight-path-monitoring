#' read a kml file into an r object
#'
#' @param file character string of filename
#' @return sf object
#' @import sf xml2 data.table
#' @export
#'
read_KML <- function(file) {
  res <- list()
  nodes <- xml2::read_xml(file) |>
    xml2::xml_find_all(xpath = "//d1:Placemark")

  tracks <- {
    linestring_node <- nodes |> xml2::xml_find_first("//d1:LineString")
    name <- linestring_node |>
      xml2::xml_parent() |>
      xml2::xml_child("d1:name") |>
      xml2::xml_text()
    time <- linestring_node |>
      xml2::xml_parent() |>
      xml2::xml_child("d1:TimeStamp") |>
      xml2::xml_child("d1:when") |>
      xml2::xml_text() |>
      as.POSIXlt(tz = "UTC", format = "%Y-%m-%dT%H:%M:%OS") |>
      as.POSIXct()
    linestring_node |>
      xml2::xml_child("d1:coordinates") |>
      xml2::xml_text() |>
      strsplit(" ", TRUE) |>
      lapply(strsplit, split = ",") |>
      .subset2(1) |>
      do.call(rbind, args = _) |>
      apply(2, as.numeric) |>
      list() |>
      sf::st_multilinestring("XYZ") |>
      sf::st_geometry() |>
      sf::st_sf(name = name, time = time, geometry = _) |>
      sf::st_set_crs(4326)
  }

  track_points <- {
    track_node <- nodes |> xml2::xml_find_first("//gx:Track")
    time <- track_node |>
      xml2::xml_find_all("d1:when") |>
      xml2::xml_text() |>
      as.POSIXlt(tz = "UTC", format = "%Y-%m-%dT%H:%M:%OS") |>
      as.POSIXct()
    track_node |>
      xml2::xml_find_all("gx:coord") |>
      xml2::xml_text() |>
      strsplit(split = " ") |>
      do.call(rbind, args = _) |>
      apply(2, as.numeric) |>
      data.table::as.data.table() |>
      data.table::setnames(c("x", "y", "ele")) |>
      data.table::set(j = "track_seg_point_id", value = seq_len(length(time))) |>
      data.table::set(j = "time", value = time) |>
      sf::st_as_sf(coords = c("x", "y"), crs = 4326)
  }

  return(list(tracks = tracks, track_points = track_points))
}
