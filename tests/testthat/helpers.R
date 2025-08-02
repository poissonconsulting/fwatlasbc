save_csv <- function(x) {
  path <- tempfile(fileext = ".csv")
  readr::write_csv(x, path)
  path
}

expect_snapshot_data <- function(x, name) {
  x <- dplyr::as_tibble(x)
  x$geometry <- NULL
  testthat::skip_on_os("windows")
  path <- save_csv(x)
  testthat::expect_snapshot_file(path, paste0(name, ".csv"))
}

skip_on_runiverse <- function() {
  if (nzchar(Sys.getenv("UNIVERSE_NAME"))) {
    testthat::skip("Skipping test on R-universe")
  }
}
