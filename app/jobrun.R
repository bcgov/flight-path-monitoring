# Email to come, have to figure out a way to send out email from shinyapps.io since it is not
# officially supported, have to use own smtp server.

# Run Flight Analysis
library(flight.path.monitoring)

args <- commandArgs(TRUE)
jobpath <- args[1]
job <- jobpath |> basename()
email <- args[2]

# Send initial email

##### initial email ####

# Start job
print(paste("Job Start time :", Sys.time()))

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
results <- process_flight(files, viewshed = FALSE)

# Save results
saveRDS(results, file.path(jobpath, "results.rds"))

# Job completed
print(paste("Job Completed time :", Sys.time()))

# Send terminal email

##### terminal email ####
