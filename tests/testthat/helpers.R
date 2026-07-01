save_csv <- function(x) {
  path <- tempfile(fileext = ".csv")
  readr::write_csv(x, path)
  path
}

expect_snapshot_data <- function(x, name, digits = 4) {
  x <- dplyr::as_tibble(x)
  x$geometry <- NULL
  fun <- function(x) {
    if (!is.double(x)) return(x)
    if (all(is.na(x) | x == round(x))) return(x)
    signif(x, digits = digits)
  }
  lapply_fun <- function(x) I(lapply(x, fun))
  x <- dplyr::mutate(x, dplyr::across(where(is.numeric), fun))
  x <- dplyr::mutate(x, dplyr::across(where(is.list), lapply_fun))
  path <- save_csv(x)

  testthat::expect_snapshot_file(
    path,
    paste0(name, ".csv"),
    compare = testthat::compare_file_text
  )
}


skip_on_runiverse <- function() {
  if (nzchar(Sys.getenv("UNIVERSE_NAME"))) {
    testthat::skip("Skipping test on R-universe")
  }
}
