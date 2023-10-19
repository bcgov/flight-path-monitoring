# Email to come, have to figure out a way to send out email from shinyapps.io since it is not
# officially supported, have to use own smtp server.

options("flight.path.monitoring.use.parallel" = FALSE)

# Set storage
options("flight.path.monitoring.cloud.storage" = TRUE)
source("storage.R", local = TRUE)
storage <- storage_backend()

# Run Flight Analysis
library(flight.path.monitoring)
library(blastula)

args <- commandArgs(TRUE)
jobpath <- args[1]
job <- jobpath |> basename()
email <- args[2]
viewshed <- args[3] |> as.logical()

# Start job
print(paste("Job Start time :", Sys.time()))
print(ifelse(viewshed, "Viewshed analysis activated", "Viewshed analysis disabled"))
print("Submitted Job ID : %s" |> sprintf(job))

# Send initial email
print("Signaling user : %s" |> sprintf(email))
tryCatch({
  compose_email(
    body = md("
A flight path analysis is in process.

A second email will be sent once the analysis job has completed.

Job ID : %s" |> sprintf(job)),
  ) |> smtp_send(
    to = email,
    from = "flight.path.analysis@gmail.com",
    subject = "Flight path analysis job submitted : %s" |> sprintf(job),
    credentials = smtpconf()
  )
}, error = function(e) print(e))

# Unzip zip files
zips <- list.files(jobpath, "\\.zip$", ignore.case = TRUE, full.names = TRUE)
if (length(zips)) {
  i <- 0
  for (zip in zips) {
    i <- i + 1
    files <- unzip(zip, list = TRUE)
    files <- files[(tools::file_ext(files$Name) |> tolower()) %in% c("gpx", "kml") |> which(),]
    unzip(zip, files = files$Name, exdir = file.path(jobpath, paste0("zip", i)))
  }
}

# Process flights
files <- list.files(jobpath, "\\.gpx$|\\.kml$", ignore.case = TRUE, full.names = TRUE, recursive = TRUE)

print("User submitted flights [%s]:" |> sprintf(length(files)))
for (file in files) {
  print(file)
}
print("Processing flights...")
results <- process_flight(files, viewshed = viewshed)

# Save results
storage$saveRDS(results, file.path(jobpath, "results.rds"))

# Send terminal email
tryCatch({
  compose_email(
    body = md("
A flight path analysis has completed.

Results can be download from the Analysis Job Results panel using the Job ID.

Job ID : %s" |> sprintf(job)),
  ) |> smtp_send(
    to = email,
    from = "flight.path.analysis@gmail.com",
    subject = "Flight path analysis job complete : %s" |> sprintf(job),
    credentials =  smtpconf()
  )
}, error = function(e) print(e))


# Job completed
print(paste("Job Completed time :", Sys.time()))
list.files(jobpath, "\\.log$", ignore.case = TRUE, full.names = TRUE) |> storage$cloudsync(file = _)
