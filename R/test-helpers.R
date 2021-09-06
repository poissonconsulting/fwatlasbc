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
