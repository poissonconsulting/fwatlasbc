test_that("fwa_add_cut_to_rms simple example", {
  x <- data.frame(blk = 1L, rm = seq(1000L, 10000L, by = 1000L))
  y <- data.frame(
    blk = 1L,
    rm_start = c(3000L, 7500L, 9000L),
    rm_end = c(4000L, 8500L, 100000L), cut = c("3", "7", "10")
  )

  x <- fwa_add_cut_to_rms(x, y)
  expect_s3_class(x, "tbl")
  expect_identical(colnames(x), c("blk", "rm", "cut"))
  expect_identical(x$blk, rep(1L, 10))
  expect_identical(x$rm, seq(1000L, 10000L, by = 1000L))
  expect_identical(x$cut, c(NA, NA, "3", "3", NA, NA, NA, "7", "10", "10"))
})

test_that("fwa_add_cut_to_rms preserves order", {
  x <- data.frame(blk = 1L, rm = rev(seq(1000L, 10000L, by = 1000L)))
  y <- data.frame(
    blk = 1L,
    rm_start = c(3000L, 7500L, 9000L),
    rm_end = c(4000L, 8500L, 100000L), cut = c("3", "7", "10")
  )

  x <- fwa_add_cut_to_rms(x, y)
  expect_s3_class(x, "tbl")
  expect_identical(colnames(x), c("blk", "rm", "cut"))
  expect_identical(x$blk, rep(1L, 10))
  expect_identical(x$rm, rev(seq(1000L, 10000L, by = 1000L)))
  expect_identical(x$cut, c("10", "10", "7", NA, NA, NA, "3", "3", NA, NA))
})

test_that("fwa_add_cut_to_rms adds missing values if zero length", {
  x <- data.frame(blk = 1L, rm = seq(1000L, 10000L, by = 1000L))
  y <- data.frame(
    blk = 1L,
    rm_start = c(3000L, 7500L, 9000L),
    rm_end = c(4000L, 8500L, 100000L), new = c("3", "7", "10")
  )

  y$new <- NA_real_
  x <- fwa_add_cut_to_rms(x, y[0, ], cut = "new")
  expect_s3_class(x, "tbl")
  expect_identical(colnames(x), c("blk", "rm", "new"))
  expect_identical(x$blk, rep(1L, 10))
  expect_identical(x$rm, seq(1000L, 10000L, by = 1000L))
  expect_identical(x$new, rep(NA_real_, 10))
})

test_that("fwa_add_cut_to_rms adds zero length", {
  x <- data.frame(blk = 1L, rm = seq(1000L, 10000L, by = 1000L))
  y <- data.frame(
    blk = 1L,
    rm_start = c(3000L, 7500L, 9000L),
    rm_end = c(4000L, 8500L, 100000L), new = c("3", "7", "10")
  )
  x <- x[0, ]
  y <- y[0, ]
  y$cut <- character(0)
  x <- fwa_add_cut_to_rms(x, y)
  expect_s3_class(x, "tbl")
  expect_identical(colnames(x), c("blk", "rm", "cut"))
  expect_identical(x$blk, integer(0))
  expect_identical(x$rm, integer(0))
  expect_identical(x$cut, character(0))
})

test_that("fwa_add_cut_to_rms handles missing section values", {
  x <- data.frame(blk = 1L, rm = seq(1000L, 10000L, by = 1000L))
  y <- data.frame(
    blk = 1L,
    rm_start = c(3000L, 7500L, 9000L),
    rm_end = c(4000L, 8500L, 100000L), cut = c(NA, "7", "10")
  )

  x <- fwa_add_cut_to_rms(x, y)
  expect_s3_class(x, "tbl")
  expect_identical(colnames(x), c("blk", "rm", "cut"))
  expect_identical(x$blk, rep(1L, 10))
  expect_identical(x$rm, seq(1000L, 10000L, by = 1000L))
  expect_identical(x$cut, c(NA, NA, NA, NA, NA, NA, NA, "7", "10", "10"))
})

test_that("fwa_add_cut_to_rms doesn't grab extra column", {
  x <- data.frame(blk = 1L, rm = seq(1000L, 10000L, by = 1000L))
  y <- data.frame(
    blk = 1L,
    rm_start = c(3000L, 7500L, 9000L),
    rm_end = c(4000L, 8500L, 100000L), cut = c("3", "7", "10"),
    extra = 1:3
  )

  x <- fwa_add_cut_to_rms(x, y)
  expect_s3_class(x, "tbl")
  expect_identical(colnames(x), c("blk", "rm", "cut"))
  expect_identical(x$blk, rep(1L, 10))
  expect_identical(x$rm, seq(1000L, 10000L, by = 1000L))
  expect_identical(x$cut, c(NA, NA, "3", "3", NA, NA, NA, "7", "10", "10"))
})

test_that("fwa_add_cut_to_rms sf", {
  x <- dplyr::tribble(
    ~blk, ~rm, ~parent_blk, ~parent_rm, ~x, ~y, ~popn,
    1, 0, NA_integer_, NA_integer_, 0, 0, "p1",
    1, 1, NA_integer_, NA_integer_, 1, 0, "p2",
    1, 2, NA_integer_, NA_integer_, 2, 0, "p3",
    1, 3, NA_integer_, NA_integer_, 3, 0, "p4",
    2, 0, 1, 0.5, 0.5, 0, "pa",
    2, 1, 1, 0.5, 0.5, 1, "pb",
    2, 2, 1, 0.5, 0.5, 2, "pc"
  ) |>
    sf::st_as_sf(coords = c("x", "y"), dim = "XY")

  y <- data.frame(
    blk = 1L,
    rm_start = c(1L, 3L),
    rm_end = c(2L, 10L),
    cut = c(TRUE, FALSE)
  )


  x <- fwa_add_cut_to_rms(x, y)
  expect_s3_class(x, "sf")
  expect_identical(colnames(x), c(
    "blk", "rm", "parent_blk", "parent_rm", "popn", "geometry",
    "cut"
  ))
  expect_identical(nrow(x), 7L)
  expect_identical(x$cut, c(NA, TRUE, TRUE, FALSE, NA, NA, NA))

  expect_snapshot_data(x, "addsectionsf")
})

test_that("fwa_add_cut_to_rms sf preserve geometry2", {
  x <- dplyr::tribble(
    ~blk, ~rm, ~parent_blk, ~parent_rm, ~x, ~y, ~popn,
    1, 0, NA_integer_, NA_integer_, 0, 0, "p1",
    1, 1, NA_integer_, NA_integer_, 1, 0, "p2",
    1, 2, NA_integer_, NA_integer_, 2, 0, "p3",
    1, 3, NA_integer_, NA_integer_, 3, 0, "p4",
    2, 0, 1, 0.5, 0.5, 0, "pa",
    2, 1, 1, 0.5, 0.5, 1, "pb",
    2, 2, 1, 0.5, 0.5, 2, "pc"
  ) |>
    sf::st_as_sf(coords = c("x", "y"), dim = "XY") |>
    dplyr::mutate(geometry2 = geometry) |>
    sf::st_set_geometry("geometry2") |>
    sf::st_set_crs(4326)

  y <- data.frame(
    blk = 1L,
    rm_start = c(1L, 3L),
    rm_end = c(2L, 10L),
    cut = c(TRUE, FALSE)
  )

  x <- fwa_add_cut_to_rms(x, y)
  expect_s3_class(x, "sf")
  expect_identical(colnames(x), c(
    "blk", "rm", "parent_blk", "parent_rm", "popn", "geometry", "geometry2",
    "cut"
  ))
  expect_identical(nrow(x), 7L)
  expect_identical(x$cut, c(NA, TRUE, TRUE, FALSE, NA, NA, NA))
  expect_identical(sf::st_geometry(x), x$geometry2)
})
