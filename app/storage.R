exec_with_retries <- function(expr, retries = 2L) {
  attempt <- 1L
  while (attempt <= retries) {
    no_error <- TRUE
    res <- tryCatch(
      eval.parent(substitute(expr)),
      error = function(e) {
        message(sprintf("Error encountered : %s", e |> as.character()))
        no_error <<- FALSE
      }
    )
    if (no_error) break
    message(paste("Retrying attemps", attempt, "in", 3^attempt, "seconds."))
    Sys.sleep(3^attempt)
    attempt <- attempt + 1L
    if (attempt > retries) {
      stop("Expression could not be evaluated.")
    }
  }
  return(res)
}

storage_backend <- function() {

  if (isTRUE(getOption("flight.path.monitoring.cloud.storage"))) {

    if (!requireNamespace("googledrive", quietly = TRUE)) {
      install.packages("googledrive")
    }

    options(gargle_oauth_cache = "./gargle")
    options(googledrive_quiet = TRUE)
    googledrive::drive_auth(email = "flight.path.analysis@gmail.com")

    cloud_exists <- function(pattern) {
      function(paths) {
        vapply(paths, function(path) {
          isTRUE(
            nrow(
              exec_with_retries({
                res <- googledrive::drive_get(path)
                res[grepl(pattern, res$path),]
              })
            ) > 0
          )
        }, logical(1), USE.NAMES = FALSE)
      }
    }

    cloud_read <- function(fun) {
      function(path, ...) {
        if (isTRUE(file.exists(path))) {
          f <- path
        } else {
          f <- tempfile()
          on.exit(unlink(f))
          exec_with_retries(
            googledrive::drive_download(path, f, overwrite = TRUE)
          )
        }
        fun(f, ...)
      }
    }

    cloud_mkdir <- function(path, ...) {
      dir.create(path, ...)
      if (!cloud_exists("/$")(path)) {
        exec_with_retries(
          googledrive::drive_mkdir(path)
        )
      }
      invisible()
    }

    cloud_save <- function(object = NULL, file, ...) {
       if (!is.null(object)) {
         saveRDS(object, file, ...)
       }
       exec_with_retries(
         googledrive::drive_upload(file, file)
       )
       invisible()
    }

    list(
      "dir.exists" = function(...) {dir.exists(...) | cloud_exists("/$")(...)},
      "file.exists" = function(...) {file.exists(...) | cloud_exists("[^/]$")(...)},
      "dir.create" = cloud_mkdir,
      "readLines" = cloud_read(readLines),
      "readRDS" = cloud_read(readRDS),
      "saveRDS" = cloud_save,
      "cloudsync" = cloud_save
    )

  } else {

    list(
      "dir.exists" = dir.exists,
      "file.exists" = file.exists,
      "dir.create" = dir.create,
      "readLines" = readLines,
      "readRDS" = readRDS,
      "saveRDS" = saveRDS,
      "cloudsync" = \(...) invisible()
    )

  }

}
