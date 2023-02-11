library(shiny)
library(shinydashboard)
library(DT)
#library(flight.path.monitoring)

ui <- dashboardPage(
  dashboardHeader(title = 'Flight Path Monitoring'),

  dashboardSidebar(
    fileInput("FlightPathFile", "Upload a new flight path", accept = c(".gpx", ".zip", ".kml"))
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
    req(input$FlightPathFile)

    ext <- tools::file_ext(input$upload$name)

    switch(ext,
           gpx = flight.path.monitoring::read_GPX(input$upload$datapath),
           kml = flight.path.monitoring::read_GPX(input$upload$datapath), #TODO parse kml function
           zip = flight.path.monitoring::read_GPX(input$upload$datapath), #TODO unzip
           validate("Invalid file; Please upload a .csv or .tsv file")
    )
  })

  output$contents <- renderTable({
    file <- input$FlightPathFile
    ext <- tools::file_ext(file$datapath)

    req(file)
    validate(need(ext %in% c("gpx", "kml", "zip"), "Please upload either a gpx, kml or zip file"))

    read.csv(file$datapath, header = input$header)
  })

  output$FlightPlot <- renderPlot({
    # Plot selected rows for List Of Flights. Use 1st observation if none selected.
    sel <- ifelse(is.null(input$ListOfFlights_rows_selected),
                  1L,
                  input$ListOfFlights_rows_selected)
    plot(iris$Sepal.Length[sel])
  })

  output$ListOfFlights <- DT::renderDT(iris, )
}

shinyApp(ui, server)
