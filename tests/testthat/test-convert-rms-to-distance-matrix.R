make_distance_streams <- function() {
  data.frame(
    blk = c(1L, 2L, 3L, 4L, 5L),
    parent_blk = c(NA, 1L, 2L, 1L, NA),
    parent_rm = c(NA, 400, 100, 700, NA)
  )
}

make_distance_rms <- function() {
  sf::st_sf(
    blk = c(1L, 1L, 2L, 3L, 4L, 5L),
    rm = c(100, 500, 50, 20, 0, 10),
    geometry = sf::st_sfc(rep(list(sf::st_point(c(0, 0))), 6), crs = 3005)
  )
}

test_that("fwa_convert_rms_to_distance_matrix calculates distances", {
  x <- fwa_convert_rms_to_distance_matrix(
    make_distance_rms(),
    make_distance_streams()
  )
  expect_identical(
    x,
    matrix(
      c(
        0,
        400,
        350,
        420,
        600,
        Inf,
        400,
        0,
        150,
        220,
        200,
        Inf,
        350,
        150,
        0,
        70,
        350,
        Inf,
        420,
        220,
        70,
        0,
        420,
        Inf,
        600,
        200,
        350,
        420,
        0,
        Inf,
        Inf,
        Inf,
        Inf,
        Inf,
        Inf,
        0
      ),
      nrow = 6,
      byrow = TRUE
    )
  )
})

test_that("fwa_convert_rms_to_distance_matrix direction", {
  x <- fwa_convert_rms_to_distance_matrix(
    make_distance_rms(),
    make_distance_streams(),
    direction = TRUE
  )
  expect_identical(
    x,
    matrix(
      c(
        0,
        400,
        350,
        420,
        600,
        Inf,
        -400,
        0,
        150,
        220,
        200,
        Inf,
        -350,
        150,
        0,
        70,
        350,
        Inf,
        -420,
        220,
        -70,
        0,
        420,
        Inf,
        -600,
        -200,
        350,
        420,
        0,
        Inf,
        Inf,
        Inf,
        Inf,
        Inf,
        Inf,
        0
      ),
      nrow = 6,
      byrow = TRUE
    )
  )
})

test_that("fwa_convert_rms_to_distance_matrix accepts data frames", {
  rms <- dplyr::as_tibble(make_distance_rms())[c("blk", "rm")]
  x <- fwa_convert_rms_to_distance_matrix(rms, make_distance_streams())
  expect_identical(x[1:2, 1:2], matrix(c(0, 400, 400, 0), nrow = 2))
})

test_that("fwa_convert_rms_to_distance_matrix accepts duplicate stream rows", {
  streams <- make_distance_streams()
  streams <- rbind(streams, streams[2, ])
  x <- fwa_convert_rms_to_distance_matrix(make_distance_rms(), streams)
  expect_identical(x[1, 3], 350)
})

test_that("fwa_convert_rms_to_distance_matrix parent not in network", {
  streams <- data.frame(
    blk = c(2L, 4L),
    parent_blk = c(1L, 1L),
    parent_rm = c(400, 700)
  )
  rms <- data.frame(blk = c(2L, 4L), rm = c(50, 0))
  x <- fwa_convert_rms_to_distance_matrix(rms, streams, direction = TRUE)
  expect_identical(x, matrix(c(0, 350, 350, 0), nrow = 2))
})

test_that("fwa_convert_rms_to_distance_matrix zero rows", {
  rms <- data.frame(blk = integer(), rm = numeric())
  x <- fwa_convert_rms_to_distance_matrix(rms, make_distance_streams())
  expect_identical(x, matrix(numeric(), nrow = 0, ncol = 0))
})

test_that("fwa_convert_rms_to_distance_matrix errors blk not in streams", {
  rms <- make_distance_rms()
  rms$blk[1] <- 6L
  expect_error(
    fwa_convert_rms_to_distance_matrix(rms, make_distance_streams()),
    "must have values matching"
  )
})

test_that("fwa_convert_rms_to_distance_matrix errors negative rm", {
  rms <- make_distance_rms()
  rms$rm[1] <- -1
  expect_error(
    fwa_convert_rms_to_distance_matrix(rms, make_distance_streams()),
    "must have values greater than or equal to 0"
  )
})

test_that("fwa_convert_rms_to_distance_matrix errors negative parent_rm", {
  streams <- make_distance_streams()
  streams$parent_rm[2] <- -1
  expect_error(
    fwa_convert_rms_to_distance_matrix(make_distance_rms(), streams),
    "must have values greater than or equal to 0"
  )
})

test_that("fwa_convert_rms_to_distance_matrix errors inconsistent parents", {
  streams <- make_distance_streams()
  streams <- rbind(
    streams,
    data.frame(blk = 2L, parent_blk = 4L, parent_rm = 1)
  )
  expect_error(
    fwa_convert_rms_to_distance_matrix(make_distance_rms(), streams),
    "Each blk in `streams` must have one parent_blk and parent_rm value"
  )
})

test_that("fwa_convert_rms_to_distance_matrix errors missing parent_rm", {
  streams <- make_distance_streams()
  streams$parent_rm[2] <- NA
  expect_error(
    fwa_convert_rms_to_distance_matrix(make_distance_rms(), streams),
    "must be missing if and only if"
  )
})

test_that("fwa_convert_rms_to_distance_matrix errors circular network", {
  streams <- make_distance_streams()
  streams$parent_blk[1] <- 3L
  streams$parent_rm[1] <- 10
  expect_error(
    fwa_convert_rms_to_distance_matrix(make_distance_rms(), streams),
    "circular"
  )
})
