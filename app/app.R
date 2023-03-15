library(shiny)
library(shinydashboard)
library(DT)
library(flight.path.monitoring)
library(leaflet)

flightPathDir <- file.path("data-raw", "Heli data", "NEH", "2021")
habitat_areas <- readRDS(file.path("data-raw", "Habitat", "habitat_areas.rds"))
wildlife_telemetry <- readRDS(file.path("data-raw", "Habitat", "wildlife_telemetry.rds"))

incursion_zones_distance <- distances(High = 1500, Moderate = 1000, Low = 500, reflabel = "In UWR")



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

  processed_flight <- reactive({
    process_flight(flight = flight(),
                   zones = habitat_areas,
                   dist = distances(low = 1500, moderate = 1000, high = 500, reflabel = "in_UWR"),
                   geom_out = TRUE)
    })

  output$flight_map <- renderLeaflet({
    leaflet() |>
      addProviderTiles(provider = "Esri.WorldTopoMap") |>
      addPolygons(data = processed_flight()[["zones"]][["in_UWR"]], color = "white", opacity = 1, weight = 1, fillColor = "#db0f27", fillOpacity = 0.35) |>
      addPolygons(data = processed_flight()[["zones"]][["high"]], color = "white", opacity = 1, weight = 1, fillColor = "#db0f27", fillOpacity = 0.275) |>
      addPolygons(data = processed_flight()[["zones"]][["moderate"]], color = "white", opacity = 1, weight = 1, fillColor = "#db0f27", fillOpacity = 0.2) |>
      addPolygons(data = processed_flight()[["zones"]][["low"]], color = "white", opacity = 1, weight = 1, fillColor = "#db0f27", fillOpacity = 0.125) |>
      addPolylines(data = processed_flight()[["flight"]], weight = 1, color = "darkgreen", dashArray = 4) |>
      addPolylines(data = processed_flight()[["segments"]][["in_UWR"]], weight = 2, color = "darkblue", opacity = 1) |>
      addPolylines(data = processed_flight()[["segments"]][["high"]], weight = 2, color = "blue", opacity = 1) |>
      addPolylines(data = processed_flight()[["segments"]][["moderate"]], weight = 2, color = "cornflowerblue", opacity = 1) |>
      addPolylines(data = processed_flight()[["segments"]][["low"]], weight = 2, color = "skyblue", opacity = 1) |>
      addPolylines(data = processed_flight()[["segments"]][["filtered"]], weight = 2, color = "deeppink", opacity = 1)
  })


  output$ListOfFlights <- DT::renderDT(as.data.frame(list.files(flightPathDir)),
                                       selection = list(mode = 'single', selected = 1L))
}

shinyApp(ui, server)
