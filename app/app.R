library(shiny)
library(shinydashboard)
library(DT)
library(flight.path.monitoring)

flightPathDir <- file.path("data-raw", "Heli data", "NEH", "2021")

ui <- dashboardPage(
  dashboardHeader(title = 'Flight Path Monitoring'),

  dashboardSidebar(
    fileInput("upload", "Upload a new flight path", accept = c(".gpx", ".zip", ".kml"), multiple = FALSE) #TODO allow multiple at some point
  ),

  dashboardBody(

    fluidRow(
      box(plotOutput("FlightPlot", height = 250))
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

    ext <- tools::file_ext(input$upload$name)

    switch(ext,
           gpx = flight.path.monitoring::read_GPX(input$upload$datapath),
           kml = flight.path.monitoring::read_GPX(input$upload$datapath), #TODO parse kml function
           zip = flight.path.monitoring::read_GPX(input$upload$datapath), #TODO unzip
           validate("Invalid file; Please upload a .gpx,  .kml or .zip file")
           )

    file.copy(from = file$datapath,
              to = file.path(flightPathDir, input$name),
              overwrite = FALSE)
  })


  output$FlightPlot <- renderPlot({
    # Plot selected rows for List Of Flights. Use 1st observation if none selected.
    sel <- ifelse(is.null(input$ListOfFlights_rows_selected),
                  1L,
                  input$ListOfFlights_rows_selected)
    plot(iris$Sepal.Length[sel])
    ggplot() +
      geom_sf(data = flight$tracks |> sf::st_geometry(), colour = "lightgreen")
  })

  output$ListOfFlights <- DT::renderDT(as.data.frame(list.files(flightPathDir)))
}

shinyApp(ui, server)
