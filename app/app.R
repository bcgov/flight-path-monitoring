library(shiny)
library(shinyvalidate)
library(flight.path.monitoring)

flightJobsDir <- file.path(tempdir(), "flight_jobs")
dir.create(flightJobsDir, showWarnings = FALSE)

jobid <- function(n) {
  c(48:57, 65:90) |> sample(replace = TRUE, size = n) |> as.raw() |> rawToChar()
}

ui <- navbarPage(

  title = HTML('&nbsp;&nbsp;<img src="logo.svg" class="navbar-logo">'),

  theme = bslib::bs_theme(version = 5, bootswatch = "yeti", "navbar-light-bg" = "#003366"),

  windowTitle = "Flight Path Monitoring",

  tabPanel("New Job",

    sidebarLayout(

      sidebarPanel(

        tags$h4("Submit Flights for Analysis"),

        tags$br(),

        fileInput(
          "upload", "Choose or drop flights (.gpx, .zip, .kml)",
          accept = c(".gpx", ".zip", ".kml"), multiple = TRUE
        ),

        textInput("email", "Job alert email", placeholder = "user@gov.bc.ca"),

        tags$br(),

        actionButton("launchjob", "Start Analysis", icon = icon("helicopter"), width = "100%")

      ),

      mainPanel(

        uiOutput("joblaunched")

      )

    )

  ),

  tabPanel("Job Results",

    sidebarLayout(

      sidebarPanel(

        textInput("jobid", "Job ID", placeholder = jobid(10)),

        actionButton("retrievejob", "Retrieve Job Results", icon = icon("table-list"), width = "100%")

      ),

      mainPanel(

        uiOutput("jobresults")

      )

    )

  )

)

server <- function(input, output, session) {

  options(shiny.maxRequestSize = 100 * 1024^2)
  iv <- InputValidator$new()
  iv$add_rule("email", sv_email())



}

shinyApp(ui, server)
