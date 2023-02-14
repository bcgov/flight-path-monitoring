compute_loi <- function(poi, zoi) {

  # Transform points of interest to LINESTRING of interest
  # CRS transform is done after the union to avoid extra
  # computing cost of not using WSG 84 (s2 package)
  loi <- sf::st_union(
    sf::st_geometry(poi)[-nrow(poi)],
    sf::st_geometry(poi)[-1],
    by_feature = TRUE
  ) |>
    sf::st_cast("LINESTRING") |>
    sf::st_as_sf() |>
    sf::st_transform(crs = sf::st_crs(zoi))

  # Won't be using 3D distance as length calculation are for relative
  # ratio only, 3D would not have a large enough impact on hypothesis

  # Add line of interest metadata, above ground levels, time deltas
  # Keep point id for easier debugging, and in zone flag (to remove
  # LINESTRING between points outside the zones, the resulting straight
  # line could go through an incursion zone.
  loi[["track_seg_point_id_start"]] <- poi[["track_seg_point_id"]][-nrow(poi)]
  loi[["track_seg_point_id_end"]] <- poi[["track_seg_point_id"]][-1]
  loi[["filtered"]] <- poi[["filtered"]][-nrow(poi)] & poi[["filtered"]][-1]
  loi[["outside"]] <- poi[["outside"]][-nrow(poi)] & poi[["outside"]][-1]
  loi[["time_deltas"]] <- difftime(
    poi[["time"]][-1],
    poi[["time"]][-nrow(poi)],
    units = "secs"
  )

  # Keep line of interest with both endpoints not flagged as outside the zones
  loi <- loi[which(!loi[["outside"]]),]

    # Compute original length of linestring before apply incursion zones / viewshed masks
  loi[["unmasked_length"]] <- sf::st_length(loi)

  # Set agr to avoid warning on intersection, since fields are kept constant with each feature
  sf::st_agr(loi) <- factor("constant", levels(sf::st_agr(loi)))

  return(loi)

}
