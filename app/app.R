library(shiny)
library(shinydashboard)
library(DT)
library(flight.path.monitoring)
library(leaflet)

options(shiny.maxRequestSize = 10 * 1024^2)
flightPathDir <- file.path("..", "data-raw", "Heli data", "NEH", "2021")

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
    ),
    fluidRow(
      valueBoxOutput("flight_name", width = 4),
      valueBoxOutput("flight_time_uwr", width = 2),
      valueBoxOutput("flight_time_high", width = 2),
      valueBoxOutput("flight_time_moderate", width = 2),
      valueBoxOutput("flight_time_low", width = 2)
    ),
    fluidRow(
      verbatimTextOutput('logs')
    )

  )
)

server <- function(input, output) {

  output$logs <- renderPrint({
    req(input$upload)
    file_to_copy <- input$upload
    cat("Reading file:", file_to_copy$name, "\n")
    cat("size:", file_to_copy$size, " Bytes\n")
    cat("copying to :", file.path(flightPathDir, file_to_copy$name), "\n")
    file.copy(from = file_to_copy$datapath,
              to = file.path(flightPathDir, file_to_copy$name),
              overwrite = FALSE)
  })

  flight_row <- reactive({ifelse(is.null(input$ListOfFlights_rows_selected),
                                      1L,
                                      input$ListOfFlights_rows_selected)})

  flight <- reactive({
    withProgress(message = "Read Flight", value = 0, {
      read_flight(list.files(flightPathDir, full.names = TRUE)[flight_row()])
    })
  })

  processed_flight <- reactive({
    withProgress(message = "Processing Flight", value = 0, {
      process_flight(flight = flight(), geom_out = TRUE)
      })
    }) |> bindCache(list.files(flightPathDir, full.names = TRUE)[flight_row()])

  output$flight_map <- renderLeaflet({
    plot(processed_flight())
  })

  #output$flight_stats <- DT::renderDT(processed_flight()[["summary"]])

  output$flight_name <- renderValueBox(valueBox(processed_flight()[["summary"]][["name"]], subtitle = "Flight Name"))
  output$flight_time_uwr <- renderValueBox(valueBox(value = round(processed_flight()[["summary"]][["in_uwr"]], 2),
                                                    subtitle = "In UWR", color = "red"))
  output$flight_time_high <- renderValueBox(valueBox(value = round(processed_flight()[["summary"]][["high"]], 2),
                                                     subtitle = "High", color = "orange"))
  output$flight_time_moderate <- renderValueBox(valueBox(value = round(processed_flight()[["summary"]][["moderate"]], 2),
                                                         subtitle = "Moderate", color = "yellow"))
  output$flight_time_low <- renderValueBox(valueBox(value = round(processed_flight()[["summary"]][["low"]], 2),
                                                    subtitle = "Low", color = "green"))
  #output$flight_time_all <- renderValueBox(valueBox(value = round(processed_flight()[["summary"]][["all"]], 2),
  #                                                  subtitle = "All"))

  list_of_flights <- reactive({
    list.files(flightPathDir)
  }) |> bindEvent(input$upload, ignoreNULL = FALSE)

  output$ListOfFlights <- DT::renderDT(data.frame(File = list_of_flights()),
                                       selection = list(mode = 'single', selected = 1L, target = 'row'))
}

shinyApp(ui, server)
