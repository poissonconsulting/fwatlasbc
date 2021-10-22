test_that("fwa_add_section_to_rms simple example", {
  x <- data.frame(blk = 1L, rm = seq(1000L, 10000L, by = 1000L))
  y <- data.frame(blk = 1L, rm = c(3000L, 7500L, 9000L), section = c("3", "7", "10"))

  x <- fwa_add_section_to_rms(x, y)
  expect_s3_class(x, "tbl")
  expect_identical(colnames(x), c("blk", "rm", "section"))
  expect_identical(x$blk, rep(1L, 10))
  expect_identical(x$rm, seq(1000L, 10000L, by = 1000L))
  expect_identical(x$section, c("3", "3", "3", "7", "7", "7", "7", "10", "10", NA))
})

test_that("fwa_add_section_to_rms preserves order", {
  x <- data.frame(blk = 1L, rm = rev(seq(1000L, 10000L, by = 1000L)))
  y <- data.frame(blk = 1L, rm = c(3000L, 7500L, 9000L), section = 3:1)

  x <- fwa_add_section_to_rms(x, y)
  expect_s3_class(x, "tbl")
  expect_identical(colnames(x), c("blk", "rm", "section"))
  expect_identical(x$blk, rep(1L, 10))
  expect_identical(x$rm, rev(seq(1000L, 10000L, by = 1000L)))
  expect_identical(x$section, c(NA, 1L, 1L, 2L, 2L, 2L, 2L, 3L, 3L, 3L))
})

test_that("fwa_add_section_to_rms adds missing values if zero length", {
  x <- data.frame(blk = 1L, rm = seq(1000L, 10000L, by = 1000L))
  y <- data.frame(blk = 1L, rm = c(3000L, 7500L, 9000L), new = 2000L)
  y$new <- NA_real_
  x <- fwa_add_section_to_rms(x, y[0,], section = "new")
  expect_s3_class(x, "tbl")
  expect_identical(colnames(x), c("blk", "rm", "new"))
  expect_identical(x$blk, rep(1L, 10))
  expect_identical(x$rm, seq(1000L, 10000L, by = 1000L))
  expect_identical(x$new, rep(NA_real_, 10))
})

test_that("fwa_add_section_to_rms adds zero length", {
  x <- data.frame(blk = 1L, rm = seq(1000L, 10000L, by = 1000L))
  y <- data.frame(blk = 1L, rm = c(3000L, 7500L, 9000L), new = 2000L)

  x <- x[0,]
  y <- y[0,]
  y$section <- character(0)
  x <- fwa_add_section_to_rms(x, y)
  expect_s3_class(x, "tbl")
  expect_identical(colnames(x), c("blk", "rm", "section"))
  expect_identical(x$blk, integer(0))
  expect_identical(x$rm, integer(0))
  expect_identical(x$section, character(0))
})

test_that("fwa_add_section_to_rms handles missing section values", {
  x <- data.frame(blk = 1L, rm = seq(1000L, 10000L, by = 1000L))
  y <- data.frame(blk = 1L, rm = c(2000L, 3000L, 7500L, 9000L), section = c(NA, 3L, 7L, 10L))

  x <- fwa_add_section_to_rms(x, y)
  expect_s3_class(x, "tbl")
  expect_identical(colnames(x), c("blk", "rm", "section"))
  expect_identical(x$blk, rep(1L, 10))
  expect_identical(x$rm, seq(1000L, 10000L, by = 1000L))
  expect_identical(x$section, c(NA, NA, 3L, 7L, 7L, 7L, 7L, 10L, 10L, NA))
})

test_that("fwa_add_section_to_rms doesn't grab extra column", {
  x <- data.frame(blk = 1L, rm = seq(1000L, 10000L, by = 1000L))
  y <- data.frame(blk = 1L, rm = c(3000L, 7500L, 9000L), section = c(3L, 7L, 10L),
                  extra = c("x", "y", "x"))

  x <- fwa_add_section_to_rms(x, y)
  expect_s3_class(x, "tbl")
  expect_identical(colnames(x), c("blk", "rm", "section"))
  expect_identical(x$blk, rep(1L, 10))
  expect_identical(x$rm, seq(1000L, 10000L, by = 1000L))
  expect_identical(x$section, c(3L, 3L, 3L, 7L, 7L, 7L, 7L, 10L, 10L, NA))
})
