library(shiny)
library(flight.path.monitoring)

# Copy assets to cache directory
if (file.exists("habitat_areas.rds")) {
  file.rename("habitat_areas.rds", file.path(cache_dir(), "habitat_areas.rds"))
}

# Setup flight jobs directory to hold submitted fligths
flightJobsDir <- file.path(getwd(), "flight_jobs")
dir.create(flightJobsDir, showWarnings = FALSE)

# Job ID generator helper function
jobid <- function(n = 10) {
  job <- c(48:57, 65:90) |> sample(replace = TRUE, size = n) |> as.raw() |> rawToChar()
  jobpath <- file.path(flightJobsDir, job)
  if (dir.exists(jobpath)) {
    job <- jobid(n)
  } else {
    dir.create(jobpath, showWarnings = FALSE)
  }
  return(job)
}

# User interface
ui <- navbarPage(

  title = HTML('&nbsp;&nbsp;<img src="logo.svg" class="navbar-logo">'),

  id = "uiTabset",

  theme = bslib::bs_theme(version = 5, bootswatch = "yeti", "navbar-light-bg" = "#003366"),

  windowTitle = "Flight Path Monitoring",

  tabPanel("New Analysis Job",

           value = "newjob",

           tags$head(includeScript("./www/script.js")),

           sidebarLayout(

             sidebarPanel(

               tags$h4("Submit Flights for Analysis"),

               tags$br(),

               fileInput(
                 "upload", "Choose or drop flights (.gpx, .zip, .kml)",
                 accept = c(".gpx", ".zip", ".kml"), multiple = TRUE
               ),

               textInput("email", "Analysis Job Alert email", placeholder = "user@gov.bc.ca"),

               tags$br(),

               actionButton("launchjob", "Start Analysis Job", icon = icon("helicopter"), width = "100%", class = "btn-primary")

             ),

             mainPanel(

               uiOutput("joblaunched")

             )

           )

  ),

  tabPanel("Analysis Job Results",

           value = "jobresults",

           sidebarLayout(

             sidebarPanel(

               textInput("jobid", "Job ID", placeholder = "A1A1A1A1A1"),

               actionButton("retrievejob", "Retrieve Analysis Job Results", icon = icon("table-list"), width = "100%", class = "btn-primary")

             ),

             mainPanel(

               fluidPage(
                 uiOutput("jobstatus"),
                 uiOutput("jobresults")
               )

             )

           )

  )

)

# Server side
server <- function(input, output, session) {

  options(shiny.maxRequestSize = 5000 * 1024^2)

  activejob <- reactiveVal()

  results <- reactiveVal()

  validemail <- reactive({
    grepl("^\\s*[A-Z0-9._%&'*+`/=?^{}~-]+@[A-Z0-9.-]+\\.[A-Z0-9]{2,}\\s*$", as.character(input$email), ignore.case = TRUE)
  })

  validfiles <- reactive({
    v <- FALSE
    if (length(input$upload) > 0) {
      supportedExtensions <- c("gpx", "kml", "zip")
      extensions <- tools::file_ext(input$upload$name) |> tolower() |>sort() |> unique()
      if (all(extensions %in% supportedExtensions)) {
        v <- TRUE
      } else {
        unsupportedExtensions <- extensions[!extensions %in% supportedExtensions]
        session$sendCustomMessage(
          type = 'userMessage',
          message = sprintf('Unsupported file extension(s): [%s]\nSupported file extensions : [%s]', unsupportedExtensions, supportedExtensions |> paste0(collapse = ", "))
        )
      }
    }
    v
  })

  launchjobenabled <- reactive({
    validfiles() && validemail()
  })

  observe({
    if (isTRUE(launchjobenabled())) {
      updateActionButton(session, "launchjob", label = "Start Analysis Job")
      session$sendCustomMessage(type="jsCode", list(code= "$('#launchjob').prop('disabled', false)"))
    } else {
      updateActionButton(session, "launchjob", label = "Waiting for Valid Inputs")
      session$sendCustomMessage(type="jsCode", list(code= "$('#launchjob').prop('disabled', true)"))
    }
  })

  observeEvent(input$jobid, {
    if (grepl("^[A-Z0-9]{10}$", input$jobid)) {
      updateActionButton(session, "retrievejob", label = "Retrieve Analysis Job Results")
      session$sendCustomMessage(type="jsCode", list(code= "$('#retrievejob').prop('disabled', false)"))
    } else {
      updateActionButton(session, "retrievejob", label = "Waiting for Valid Analysis Job ID")
      session$sendCustomMessage(type="jsCode", list(code= "$('#retrievejob').prop('disabled', true)"))
    }
  })

  observeEvent(input$launchjob, {

    # Set Job
    activejob(jobid())
    jobpath <- file.path(flightJobsDir, activejob())

    # Retrieve parameters
    email <- input$email
    file.rename(
      input$upload$datapath,
      file.path(jobpath, input$upload$name)
    )

    # Reset Job Form state
    updateTextInput(session, "email", "Job alert email", placeholder = "user@gov.bc.ca", value = "")
    session$sendCustomMessage(type="jsCode", list(code= "$('#upload').val('')"))
    session$sendCustomMessage(type="jsCode", list(code= "$('#upload_progress').css('visibility', 'hidden')"))
    session$sendCustomMessage(type="jsCode", list(code= "$('#upload_progress').find('.progress-bar').css('width', '0')"))
    session$sendCustomMessage(type="jsCode", list(code= "$('#upload').closest('.input-group').find(\"input[type='text']\").val('')"))
    session$sendCustomMessage(type="jsCode", list(code= "Shiny.onInputChange('upload', Object())"))

    # Launch job
    system(
      sprintf(
        "Rscript jobrun.R %s %s > %s.log 2>&1",
        jobpath,
        email,
        file.path(jobpath, activejob())
      ),
      wait = FALSE
    )

    # Update main UI with message and link to results
    output$joblaunched <- renderUI({
      fluidPage(
        tags$hr(),
        fluidRow(align = "center",
          tags$br(),
          tags$br(),
          tags$h4("New flight analysis submitted."),
          tags$br(),
          tags$p("An email will be sent to ",
            tags$span(email, class = "text-primary"),
            " to confirm and another one once the analysis is completed."
          ),
          tags$br(),
          tags$br(),
          tags$h4(
            "Job ID %s" |> sprintf(activejob()),
            shiny::actionLink("copyjobidlink", "", icon = icon("copy"))
          ),
          shiny::actionButton("jobidlink", activejob(), icon = icon("table-list"), class = "btn-primary")
        ),
        tags$hr()
      )
    })

  })

  observeEvent(input$copyjobidlink, {
    session$sendCustomMessage(type="jsCode", list(code= "navigator.clipboard.writeText('%s')" |> sprintf(activejob())))
  })

  observeEvent(input$jobidlink, {
    updateTextInput(session, inputId = "jobid", value = activejob())
    updateTabsetPanel(session, "uiTabset", selected = "jobresults")
  })

  observeEvent(input$retrievejob, {

    jobpath <- file.path(flightJobsDir, input$jobid)

    jobstatus <- function(faicon, color) {
      tags$h4(
        "Job ID ",
        input$jobid,
        tags$i(
          class = "fa fa-solid fa-%s" |> sprintf(faicon),
          style = "color: %s" |> sprintf(color)
        )
      )
    }

    # Job does not exist state
    if (!dir.exists(jobpath)) {

      rowcontent <- fluidRow(
        jobstatus("circle-exclamation", "#FFA500"),
        tags$p("Analysis job does not exist or is no longer available.")
      )

      results(NULL)

    } else {

      logpath <- file.path(jobpath, "%s.log" |> sprintf(input$jobid))
      joblog <- ""
      if (file.exists(logpath)) {
        joblog <- readLines(logpath, warn = FALSE)
      }

      # Collapsible Log block
      joblogblock <- function(collapsed = FALSE) {

        tags$div(
          class = "accordion", id = "logblock",
          tags$div(
            class = "accordion-item",
            tags$h2(
              class = "accordion-header", id = "headingLoglines", style = "margin-top: 0",
              tags$button(
                class = "accordion-button%s" |> sprintf(ifelse(collapsed, " collapsed", "")), type = "button",
                `data-bs-toggle` = "collapse", `data-bs-target` = "#collapseLoglines",
                `aria-expanded` = ifelse(collapsed, "false", "true"), `aria-controls` = "collapseLoglines",
                "Analysis Job Execution Log"
              ),
              tags$div(
                id = "collapseLoglines", class = "accordion-collapse collapse%s" |> sprintf(ifelse(collapsed, "", " show")),
                `aria-labelledby` = "headingLoglines", `data-bs-parent` = "#logblock", style="",
                tags$div(
                  class = "accordion-body",
                  tags$pre(
                    style = "font-size: 0.5em",
                    lapply(joblog, \(x) {list(tags$code(x, .noWS = c("before", "after")), tags$br(.noWS = c("before", "after")))})
                  )
                )
              )
            )
          )
        )

      }

      # Error state
      if (any(grepl("^Fatal error|^Error", joblog))) {

        rowcontent <- fluidRow(
          jobstatus("circle-xmark", "#FF0000"),
          tags$p("Analysis job terminated unexpectedly."),
          joblogblock()
        )

        results(NULL)

      }
      # Job done
      else if (tail(joblog, 1L) |> grepl("[1] \"Job Completed time :", x = _, fixed = TRUE)) {

        rowcontent <- fluidRow(
          jobstatus("circle-check", "#00CC00"),
          tags$p("Analysis job completed."),
          joblogblock(collapsed = TRUE)
        )

        results(readRDS(file.path(flightJobsDir, input$jobid, "results.rds")))

      }
      # Still running
      else {

        rowcontent <- fluidRow(
          jobstatus("circle-info", "#008CBA"),
          tags$p("Analysis job is still executing."),
          joblogblock(collapsed = TRUE)
        )

        results(NULL)

      }

    }

    output$jobstatus <- renderUI({
      list(
        tags$hr(),
        rowcontent,
        tags$hr()
      )
    })

  })

  # Display results
  observe({

    if (is.null(results())) {

      output$jobresults <- renderUI({})

    } else {

      output$jobresults <- renderUI({

        renderTable(results()[["summary"]])

      })

    }

  })

}

shinyApp(ui, server)
