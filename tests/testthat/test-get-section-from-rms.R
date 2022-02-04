test_that("fwa_get_section_from_rms simple example", {
  x <- data.frame(blk = 1L, rm = seq(1000L, 10000L, by = 1000L))
  y <- data.frame(blk = 1L, rm = c(3000L, 7500L, 9000L), section = c("3", "7", "10"))

  x <- fwa_add_section_to_rms(x, y)
  z <- fwa_get_section_from_rms(x)

  expect_s3_class(z, "tbl")
  expect_identical(colnames(z), c("section", "blk", "rm", "length"))
  expect_identical(z$section, c("3", "7", "10"))
  expect_identical(z$blk, rep(1L, 3))
  expect_identical(z$rm, c(3000L, 7000L, 9000L))
  expect_identical(z$length, c(2000L, 3000L, 1000L))
})

test_that("fwa_get_section_from_rms different name", {
  x <- data.frame(blk = 1L, rm = seq(1000L, 10000L, by = 1000L))
  y <- data.frame(blk = 1L, rm = c(3000L, 7500L, 9000L), thingy = c("3", "7", "10"))

  x <- fwa_add_section_to_rms(x, y, "thingy")
  z <- fwa_get_section_from_rms(x, "thingy")

  expect_s3_class(z, "tbl")
  expect_identical(colnames(z), c("thingy", "blk", "rm", "length"))
  expect_identical(z$thingy, c("3", "7", "10"))
  expect_identical(z$blk, rep(1L, 3))
  expect_identical(z$rm, c(3000L, 7000L, 9000L))
  expect_identical(z$length, c(2000L, 3000L, 1000L))
})



test_that("fwa_get_section_from_rms sf", {
  x <- dplyr::tribble(
    ~blk, ~rm, ~parent_blk, ~parent_rm,  ~x, ~y, ~popn,
    1,     0,    NA_integer_,   NA_integer_, 0, 0, "p1",
    1,      1,    NA_integer_,   NA_integer_, 1, 0, "p2",
    1,      2,    NA_integer_,   NA_integer_, 2, 0, "p3",
    1,      3,    NA_integer_,   NA_integer_, 3, 0, "p4",
    2,     0,    1,             0.5,          0.5, 0, "pa",
    2,     1,    1,             0.5,          0.5, 1, "pb",
    2,     2,    1,             0.5,          0.5, 2, "pc") |>
    sf::st_as_sf(coords = c("x", "y"), dim = "XY")

  y <- data.frame(blk = 1L, rm = c(1, 2, 4), section = c(3L, 7L, 10L))

  x <- fwa_add_section_to_rms(x,y)
  z <- fwa_get_section_from_rms(x)

  expect_s3_class(z, "tbl")
  expect_s3_class(z, "sf")
  expect_identical(colnames(z), c("section", "blk", "rm", "length", "geometry"))
  expect_identical(z$section, c(3L, 7L, 10L))
  expect_identical(z$blk, rep(1, 3))
  expect_identical(z$rm, c(1, 2, 3))
  expect_identical(z$length, c(1, 0, 0))
  expect_snapshot_data(x, "getsectionsf")
})
