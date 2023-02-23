#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinyjs)
library(sf)
library(units)
library(terra)
library(bcdata)
library(bcmaps)
library(data.table)
library(lubridate)
library(leaflet)

ressources_path <- "/Users/nicolas/Documents/boostao/flight-path-monitoring"

read_GPX <- function(file) {
  layers_data <- sf::st_layers(file)
  non_empty <- layers_data[["name"]][which(layers_data[["features"]] > 0)] |>
    setNames(nm = _)
  lapply(non_empty, sf::st_read, dsn = file, quiet = TRUE)# |>
  #`class<-`("flightGPX")
}


flight <- read_GPX("../data-raw/Heli data/NEH/2021/20220214-162736-0031550-139533.gpx")
habitat_areas <- readRDS(paste0(ressources_path, "/habitat_areas.rds"))
wildlife_telemetry <- readRDS(paste0(ressources_path, "/wildlife_telemetry.rds"))

incursion_zones_distance <- list(
  "0-500 m incursion zone" = units::set_units(c("from" = 0L , "to" = 500L), m),
  "500-1000m incursion zone" = units::set_units(c("from" = 500L, "to" = 1000L), m),
  "1000-1500m incursion zone" = units::set_units(c("from" = 1000L, "to" = 1500L), m)
)

filter_on_bbox <- function(aoi, sf_obj) {
  sf_obj[
    which(
      .subset2(aoi, "xmin") < .subset2(sf_obj, "xmax") &
        .subset2(aoi, "xmax") > .subset2(sf_obj, "xmin") &
        .subset2(aoi, "ymin") < .subset2(sf_obj, "ymax") &
        .subset2(aoi, "ymax") > .subset2(sf_obj, "ymin")
    ),
  ]
}


read_GPX <- function(file) {
  layers_data <- sf::st_layers(file)
  non_empty <- layers_data[["name"]][which(layers_data[["features"]] > 0)] |>
    setNames(nm = _)
  lapply(non_empty, sf::st_read, dsn = file, quiet = TRUE)# |>
  #`class<-`("flightGPX")
}


compute_buffers <- function(dist, sf_obj) {

  buff_oven <- function() {
    precomputed_dist <- list(
      "0" = sf_obj |> sf::st_geometry() |> sf::st_union()
    )
    function(d) {
      d_char <- as.character(d)
      if (is.null(res <- precomputed_dist[[d_char]])) {
        precomputed_dist[[d_char]] <<- res <-
          sf::st_geometry(sf_obj) |> sf::st_buffer(d) |> sf::st_union()
      }
      return(res)
    }
  }

  bake_donut <- function(d) {
    d <- sort(d)
    buffers <- buff_oven()
    sf::st_difference(buffers(d[2]), buffers(d[1]))
  }

  if (nrow(sf_obj)) {
    return(lapply(dist, bake_donut))
  }

  return(invisible())
}

# Process one flight
zones <- habitat_areas
telemetry <- wildlife_telemetry
dist <- incursion_zones_distance


# Area of interest
aoi <- sf::st_bbox(flight[["tracks"]]) |>
  sf::st_as_sfc() |>
  sf::st_buffer(do.call(max,dist) + units::set_units(50L, m)) |>
  sf::st_transform(sf::st_crs(zones)) |>
  sf::st_bbox()

# Zones of interest
zoi <- filter_on_bbox(aoi, zones)

# Telemetry of interest
#TODO: Consider data time for telemetry?
toi <- filter_on_bbox(aoi, telemetry)

# Incursion
incursions_zoi <- compute_buffers(dist, zoi)


red_col <- "#db0f27"

blue_col <- "#d31a38"

heli_icon <- makeIcon(paste0(ressources_path, "/clipart226028.png"), iconWidth = 35, iconHeight = 25)

# Define UI for application that draws a histogram
ui <- fluidPage(
  useShinyjs(),
  sliderInput("slider",
              "animation_slider",
              min = 1,
              max = nrow(flight$track_points),
              value = 1,
              step = 10,
              animate = animationOptions(interval = 100, loop = TRUE)
  ),
  tags$script("$(document).ready(function(){
                        setTimeout(function() {$('.slider-animate-button').click()},10);
                    });"),
  leafletOutput("flight_map")
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  #hide("slider")
  output$flight_map <- renderLeaflet({
    leaflet() |>
      addProviderTiles(provider = "Esri.WorldTopoMap") |>
      addPolygons(data = st_transform(zoi, "WGS84"),color = "white", opacity = 1 , weight  = 1,fillColor = red_col, fillOpacity = 0.7) |>
      addPolygons(data = st_transform(incursions_zoi[[1]], "WGS84"), color = "white", opacity = 1 , weight  = 1, fillColor = blue_col, fillOpacity = 0.4) |>
      addPolygons(data = st_transform(incursions_zoi[[2]], "WGS84"), color = "white", opacity = 1 , weight  = 1, fillColor = blue_col, fillOpacity = 0.2) |>
      addPolygons(data = st_transform(incursions_zoi[[3]], "WGS84"), color = "white", opacity = 1 , weight  = 1, fillColor =  blue_col,  fillOpacity = 0.1) |>
      addPolylines(data = st_transform(flight$tracks, "WGS84"), weight = 1, color = "black", dashArray = 4)
  })

  map_proxy <- leafletProxy("flight_map")

  helico_point <- reactive({
    flight$track_points[input$slider,]
  })

  observe({
    map_proxy |>
      clearMarkers() |>
      addMarkers(data = helico_point(), icon = heli_icon)
  })
}

# Run the application
shinyApp(ui = ui, server = server)
