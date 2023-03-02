library(shiny)
library(shinydashboard)
library(DT)
library(flight.path.monitoring)
library(leaflet)

flightPathDir <- file.path("data-raw", "Heli data", "NEH", "2021")

ui <- dashboardPage(
  dashboardHeader(title = 'Flight Path Monitoring'),

  dashboardSidebar(
    fileInput("upload", "Upload a new flight path", accept = c(".gpx", ".zip", ".kml"), multiple = FALSE) #TODO allow multiple at some point
  ),

  dashboardBody(

    fluidRow(
      box(leafletOutput("flight_map", height = 500))
    ),

    fluidRow(
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

  output$flight_map <- renderLeaflet({
    leaflet() |>
      addProviderTiles(provider = "Esri.WorldTopoMap") |>
      addPolylines(data = st_transform(flight()$tracks, "WGS84"), weight = 1, color = "black", dashArray = 4)
  })

  map_proxy <- leafletProxy("flight_map")

  output$ListOfFlights <- DT::renderDT(as.data.frame(list.files(flightPathDir)), selection = list(mode = 'single', selected = 1L))
}

shinyApp(ui, server)
