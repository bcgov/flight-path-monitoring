#' Default buffer distances (low = 1500m, moderate = 1000m, high = 500m) using reference label
#' `in_uwr` created from a call to `distances`.
#' @export
#' @rdname defaults
default_dist <- function() {
  distances(low = 1500, moderate = 1000, high = 500, reflabel = "in_uwr")
}

#' Default legal habitat area zones as defined in the document
#' `Habitat Base Layers for Flight Monitoring Application.docx` from the github repository.
#' @param recache A boolean. Should the legal habitat area zones be retrieved from source again and
#' resaved again into the cache directory. Default to FALSE.
#' @export
#' @importFrom tools R_user_dir
#' @importFrom utils download.file unzip
#' @importFrom bcdata bcdc_get_record bcdc_query_geodata filter collect
#' @rdname defaults
default_zones <- function(recache = FALSE) {

  # Avoid package check messages `Undefined global functions or variables`
  if (FALSE) {
    SPECIES_1 <- STRGC_LAND_RSRCE_PLAN_NAME <- LEGAL_FEAT_OBJECTIVE <- NON_LEGAL_FEAT_OBJECTIVE <-
      NON_LEGAL_FEAT_ATRB_2_VALUE <- NULL
  }

  # Cache directory
  zones_dir <- tools::R_user_dir("flightpathmonitoring", "cache")
  if (!dir.exists(zones_dir)) {
    message("Creating directory to cache legal habitat area zones at ",
      zones_dir)
    dir.create(zones_dir, showWarnings = FALSE, recursive = TRUE)
  }

  # Set presaved filename
  presaved <- file.path(zones_dir, "habitat_areas.rds")

  # Retrieve legal habitat area zones
  if (!file.exists(presaved) | isTRUE(recache)) {

    # Skeena - Mountaingoat winter habitat
    tmpdir_zip <- file.path(tempdir(), "zipped")
    dir.create(tmpdir_zip, showWarnings = FALSE, recursive = TRUE)
    tmpdir <- file.path(tempdir(), "unzipped")
    utils::download.file(
      "https://github.com/bcgov/flight-path-monitoring/raw/main/data-raw/Habitat/Skeena-mountaingoatwinterhab.shp.ALGORAB.20784.25740.sr.zip",
      zipfile <- file.path(tmpdir_zip, "Skeena-mountaingoatwinterhab.shp.ALGORAB.20784.25740.sr.zip")
    )
    utils::unzip(zipfile, exdir = tmpdir)
    Skeena_mountaingoatwinterhab <- sf::st_read(tmpdir, drivers = "ESRI Shapefile", quiet = TRUE)
    Skeena_mountaingoatwinterhab[["id"]] <- paste(
      "Skeena-mountaingoatwinterhab",
      Skeena_mountaingoatwinterhab[["OBJECTID"]],
      sep = "."
    )
    # move id in first position, purely aesthetic
    n <- ncol(Skeena_mountaingoatwinterhab)
    Skeena_mountaingoatwinterhab <- Skeena_mountaingoatwinterhab[, c(n, 1L:(n-1L))]
    unlink(c(tmpdir_zip, tmpdir), recursive = TRUE)

    # Ungulate Winter Range- Approved
    # bcdc_search("Ungulate Winter Range")
    Ungulate_winterrange_approved <- bcdata::bcdc_get_record("712bd887-7763-4ed3-be46-cdaca5640cc1") |>
      bcdata::bcdc_query_geodata() |>
      bcdata::filter(SPECIES_1 %in% c("M-ORAM", "M-RATA-01", "M-RATA-15")) |>
      bcdata::collect()

    # Ungulate Winter Range- Proposed
    # bcdc_search("Ungulate Winter Range")
    Ungulate_winterrange_proposed <- bcdata::bcdc_get_record("e5c2a2e3-70fc-49e3-9840-87212853e8a2") |>
      bcdata::bcdc_query_geodata() |>
      bcdata::filter(SPECIES_1 %in% c("M-ORAM", "M-RATA-01", "M-RATA-15")) |>
      bcdata::collect()

    # Legal Planning Objectives - Current - Polygon
    # bcdc_search("Legal Planning Objectives Polygon")
    legal_habitat_areas <- bcdata::bcdc_get_record("2c02040c-d7c5-4960-8d04-dea01d6d3e9f") |>
      bcdata::bcdc_query_geodata() |>
      bcdata::filter(
        STRGC_LAND_RSRCE_PLAN_NAME == "Dease-Liard Sustainable Resource Management Plan",
        LEGAL_FEAT_OBJECTIVE == "Caribou Winter Habitat Zones"
      ) |>
      bcdata::collect()

    # Non Legal Planning Features - Current - Polygon
    # bcdc_search("Non Legal Planning Features Polygon")
    non_legal_habitat_areas <- bcdata::bcdc_get_record("5d859a89-f173-4006-82f9-16254de2c1fc") |>
      bcdata::bcdc_query_geodata() |>
      bcdata::filter(
        (
          STRGC_LAND_RSRCE_PLAN_NAME == "Dease-Liard Sustainable Resource Management Plan" &
          NON_LEGAL_FEAT_OBJECTIVE == "High Value Mountain Ungulate Habitat"
        ) |
        (
          STRGC_LAND_RSRCE_PLAN_NAME == "Lakes District Land and Resource Management Plan" &
          NON_LEGAL_FEAT_OBJECTIVE == "SRMZ3:Caribou Migration Corridor Sub-Zone" &
          NON_LEGAL_FEAT_ATRB_2_VALUE %in% c("Caribou Migration Corridor - High", "Caribou Migration Corridor - Very High")
        ) |
        (
          STRGC_LAND_RSRCE_PLAN_NAME == "Morice Land and Resource Management Plan" &
          NON_LEGAL_FEAT_OBJECTIVE %in% c("Mountain Goat Habitat Areas", "Takla Caribou Winter Range", "Tweedsmuir Caribou:Calving Islands")
        )
      ) |>
      bcdata::collect()

    habitat_areas <- rbind(
     Skeena_mountaingoatwinterhab[,"id"],
     Ungulate_winterrange_approved[,"id"],
     Ungulate_winterrange_proposed[,"id"],
     legal_habitat_areas[,"id"],
     non_legal_habitat_areas[,"id"]
    ) |> append_bbox_info()

    rm(
     Skeena_mountaingoatwinterhab,
     Ungulate_winterrange_approved,
     Ungulate_winterrange_proposed,
     legal_habitat_areas,
     non_legal_habitat_areas
    )

    saveRDS(habitat_areas, presaved)

  } else {

    habitat_areas <- readRDS(presaved)

  }

  return(habitat_areas)

}
