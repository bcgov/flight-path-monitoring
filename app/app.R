library(shiny)
library(shinydashboard)
library(DT)
library(flight.path.monitoring)
library(leaflet)

flightPathDir <- file.path("data-raw", "Heli data", "NEH", "2021")
#habitat_areas <- readRDS(paste0(ressources_path, "/habitat_areas.rds"))
#wildlife_telemetry <- readRDS(paste0(ressources_path, "/wildlife_telemetry.rds"))


ui <- dashboardPage(
  dashboardHeader(title = 'Flight Path Monitoring'),

  dashboardSidebar(
    fileInput("upload", "Upload a new flight path", accept = c(".gpx", ".zip", ".kml"), multiple = FALSE) #TODO allow multiple at some point
  ),

  dashboardBody(

    fluidRow(
      box(leafletOutput("flight_map", height = 500)),
      box(title = "Flights",
          DT::DTOutput("ListOfFlights")
          )
    )
  )
)

server <- function(input, output) {

  NewFlight <- reactive({
    req(input$upload)
    read_flights(input$upload$datapath)

    file.copy(from = file$datapath,
              to = file.path(flightPathDir, input$name),
              overwrite = FALSE)
  })

  flight_row <- reactive({ifelse(is.null(input$ListOfFlights_rows_selected),
                                      1L,
                                      input$ListOfFlights_rows_selected)})

  flight <- reactive({
    read_GPX(list.files(flightPathDir, full.names = TRUE)[flight_row()])
  })

  aoi <- reactive({
      aoi <- sf::st_bbox(flight()[["tracks"]]) |>
        sf::st_as_sfc() |>
        sf::st_buffer(do.call(max,dist) + units::set_units(50L, m)) |>
        sf::st_transform(sf::st_crs(zones)) |>
        sf::st_bbox()})

  zoi <-

  output$flight_map <- renderLeaflet({
    leaflet() |>
      addProviderTiles(provider = "Esri.WorldTopoMap") |>
      addPolylines(data = st_transform(flight()$tracks, "WGS84"), weight = 1, color = "black", dashArray = 4)
  })


  output$ListOfFlights <- DT::renderDT(as.data.frame(list.files(flightPathDir)),
                                       selection = list(mode = 'single', selected = 1L))
}

shinyApp(ui, server)
