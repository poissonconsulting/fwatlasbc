test_that("fwa_add_elevatr_elevation_to_point works", {
  skip_on_runiverse()
  skip_if_not_installed("elevatr")

  rm <- fwa_add_rms_to_blk(data.frame(blk = 356308001), interval = 1000)
  rm$fwa_elevation <- rm$elevation
  rm$elevation <- NULL
  x <- fwa_add_elevatr_elevation_to_point(rm)
  expect_s3_class(x, "sf")
  expect_identical(colnames(x), c(colnames(rm), "elevation"))
  expect_identical(x$rm, rm$rm)
  expect_type(x$elevation, "double")
  expect_false(anyNA(x$elevation))
  # terrain tiles agree with the freshwater atlas elevations to within 25 m
  expect_lt(max(abs(x$elevation - x$fwa_elevation)), 25)
})

test_that("fwa_add_elevatr_elevation_to_point zero rows", {
  skip_if_not_installed("elevatr")
  x <- sf::st_sf(geometry = sf::st_sfc(sf::st_point(c(0, 0)), crs = 3005))[0, ]
  x <- fwa_add_elevatr_elevation_to_point(x)
  expect_identical(x$elevation, numeric(0))
})

test_that("fwa_add_elevatr_elevation_to_point passes zoom", {
  skip_if_not_installed("elevatr")
  local_mocked_bindings(
    get_elev_point = function(locations, src, z, ...) {
      locations$elevation <- rep(z, nrow(locations))
      locations
    },
    .package = "elevatr"
  )
  x <- sf::st_sf(
    blk = 1:2,
    geometry = sf::st_sfc(
      sf::st_point(c(1000000, 500000)),
      sf::st_point(c(1000100, 500000)),
      crs = 3005
    )
  )
  expect_identical(fwa_add_elevatr_elevation_to_point(x)$elevation, c(12, 12))
  expect_identical(
    fwa_add_elevatr_elevation_to_point(x, zoom = 10)$elevation,
    c(10, 10)
  )
})

test_that("fwa_add_elevatr_elevation_to_point errors with invalid zoom", {
  skip_if_not_installed("elevatr")
  x <- sf::st_sf(
    geometry = sf::st_sfc(sf::st_point(c(1000000, 500000)), crs = 3005)
  )
  expect_error(fwa_add_elevatr_elevation_to_point(x, zoom = 15))
})

make_elevatr_points <- function() {
  sf::st_sf(
    blk = 1:2,
    geometry = sf::st_sfc(
      sf::st_point(c(1000000, 500000)),
      sf::st_point(c(1000100, 500000)),
      crs = 3005
    )
  )
}

test_that("fwa_add_elevatr_elevation_to_point increases connect timeout", {
  skip_if_not_installed("elevatr")
  skip_if_not_installed("httr")
  local_mocked_bindings(
    get_elev_point = function(locations, src, z, config, ...) {
      locations$elevation <- c(
        getOption("httr_config")$options$connecttimeout,
        config$options$connecttimeout
      )
      locations
    },
    .package = "elevatr"
  )
  x <- fwa_add_elevatr_elevation_to_point(make_elevatr_points())
  expect_identical(x$elevation, c(60, 60))
  rlang::local_options(fwa.elevatr_timeout = 120)
  x <- fwa_add_elevatr_elevation_to_point(make_elevatr_points())
  expect_identical(x$elevation, c(120, 120))
})

test_that("fwa_add_elevatr_elevation_to_point retries timeouts", {
  skip_if_not_installed("elevatr")
  skip_if_not_installed("httr")
  tries <- 0L
  waits <- numeric()
  local_mocked_bindings(
    get_elev_point = function(locations, ...) {
      tries <<- tries + 1L
      if (tries < 3L) {
        stop("Timeout was reached [s3.amazonaws.com]")
      }
      locations$elevation <- c(1, 2)
      locations
    },
    .package = "elevatr"
  )
  local_mocked_bindings(wait_seconds = function(seconds) {
    waits <<- c(waits, seconds)
  })
  x <- fwa_add_elevatr_elevation_to_point(make_elevatr_points())
  expect_identical(x$elevation, c(1, 2))
  expect_identical(tries, 3L)
  expect_identical(waits, c(2, 4))
})

test_that("fwa_add_elevatr_elevation_to_point errors after three timeouts", {
  skip_if_not_installed("elevatr")
  skip_if_not_installed("httr")
  tries <- 0L
  local_mocked_bindings(
    get_elev_point = function(locations, ...) {
      tries <<- tries + 1L
      stop("Timeout was reached [s3.amazonaws.com]")
    },
    .package = "elevatr"
  )
  local_mocked_bindings(wait_seconds = function(seconds) NULL)
  expect_snapshot(
    fwa_add_elevatr_elevation_to_point(make_elevatr_points()),
    error = TRUE
  )
  expect_identical(tries, 3L)
})

test_that("fwa_add_elevatr_elevation_to_point respects tries option", {
  skip_if_not_installed("elevatr")
  skip_if_not_installed("httr")
  rlang::local_options(fwa.elevatr_tries = 1, fwa.elevatr_timeout = 30)
  tries <- 0L
  local_mocked_bindings(
    get_elev_point = function(locations, ...) {
      tries <<- tries + 1L
      stop("Timeout was reached [s3.amazonaws.com]")
    },
    .package = "elevatr"
  )
  local_mocked_bindings(wait_seconds = function(seconds) NULL)
  expect_snapshot(
    fwa_add_elevatr_elevation_to_point(make_elevatr_points()),
    error = TRUE
  )
  expect_identical(tries, 1L)
})

test_that("fwa_add_elevatr_elevation_to_point errors with invalid options", {
  skip_if_not_installed("elevatr")
  skip_if_not_installed("httr")
  x <- make_elevatr_points()
  local({
    rlang::local_options(fwa.elevatr_timeout = 0)
    expect_snapshot(fwa_add_elevatr_elevation_to_point(x), error = TRUE)
  })
  local({
    rlang::local_options(fwa.elevatr_tries = 0)
    expect_snapshot(fwa_add_elevatr_elevation_to_point(x), error = TRUE)
  })
  local({
    rlang::local_options(fwa.elevatr_tries = 1.5)
    expect_snapshot(fwa_add_elevatr_elevation_to_point(x), error = TRUE)
  })
})

test_that("fwa_add_elevatr_elevation_to_point does not retry other errors", {
  skip_if_not_installed("elevatr")
  skip_if_not_installed("httr")
  tries <- 0L
  local_mocked_bindings(
    get_elev_point = function(locations, ...) {
      tries <<- tries + 1L
      stop("An AWS URL is invalid.")
    },
    .package = "elevatr"
  )
  expect_error(
    fwa_add_elevatr_elevation_to_point(make_elevatr_points()),
    "An AWS URL is invalid."
  )
  expect_identical(tries, 1L)
})
