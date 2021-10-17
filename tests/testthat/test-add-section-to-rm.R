test_that("fwa_add_section_to_rm simple example", {
  x <- data.frame(BLK = 1L, RM = seq(1000L, 10000L, by = 1000L))
  y <- data.frame(BLK = 1L, RM = c(3000L, 7500L, 9000L), Section = c("3", "7", "10"))

  x <- fwa_add_section_to_rm(x, y)
  expect_s3_class(x, "tbl")
  expect_identical(colnames(x), c("BLK", "RM", "Section"))
  expect_identical(x$BLK, rep(1L, 10))
  expect_identical(x$RM, seq(1000L, 10000L, by = 1000L))
  expect_identical(x$Section, c("3", "3", "3", "7", "7", "7", "7", "10", "10", NA))
})

test_that("fwa_add_section_to_rm preserves order", {
  x <- data.frame(BLK = 1L, RM = rev(seq(1000L, 10000L, by = 1000L)))
  y <- data.frame(BLK = 1L, RM = c(3000L, 7500L, 9000L), Section = 3:1)

  x <- fwa_add_section_to_rm(x, y)
  expect_s3_class(x, "tbl")
  expect_identical(colnames(x), c("BLK", "RM", "Section"))
  expect_identical(x$BLK, rep(1L, 10))
  expect_identical(x$RM, rev(seq(1000L, 10000L, by = 1000L)))
  expect_identical(x$Section, c(NA, 1L, 1L, 2L, 2L, 2L, 2L, 3L, 3L, 3L))
})

test_that("fwa_add_section_to_rm adds missing values if zero length", {
  x <- data.frame(BLK = 1L, RM = seq(1000L, 10000L, by = 1000L))
  y <- data.frame(BLK = 1L, RM = c(3000L, 7500L, 9000L), new = 2000L)
  y$new <- NA_real_
  x <- fwa_add_section_to_rm(x, y[0,], section = "new")
  expect_s3_class(x, "tbl")
  expect_identical(colnames(x), c("BLK", "RM", "new"))
  expect_identical(x$BLK, rep(1L, 10))
  expect_identical(x$RM, seq(1000L, 10000L, by = 1000L))
  expect_identical(x$new, rep(NA_real_, 10))
})

test_that("fwa_add_section_to_rm adds zero length", {
  x <- data.frame(BLK = 1L, RM = seq(1000L, 10000L, by = 1000L))
  y <- data.frame(BLK = 1L, RM = c(3000L, 7500L, 9000L), new = 2000L)

  x <- x[0,]
  y <- y[0,]
  y$Section <- character(0)
  x <- fwa_add_section_to_rm(x, y)
  expect_s3_class(x, "tbl")
  expect_identical(colnames(x), c("BLK", "RM", "Section"))
  expect_identical(x$BLK, integer(0))
  expect_identical(x$RM, integer(0))
  expect_identical(x$Section, character(0))
})

test_that("fwa_add_section_to_rm handles missing Section values", {
  x <- data.frame(BLK = 1L, RM = seq(1000L, 10000L, by = 1000L))
  y <- data.frame(BLK = 1L, RM = c(2000L, 3000L, 7500L, 9000L), Section = c(NA, 3L, 7L, 10L))

  x <- fwa_add_section_to_rm(x, y)
  expect_s3_class(x, "tbl")
  expect_identical(colnames(x), c("BLK", "RM", "Section"))
  expect_identical(x$BLK, rep(1L, 10))
  expect_identical(x$RM, seq(1000L, 10000L, by = 1000L))
  expect_identical(x$Section, c(NA, NA, 3L, 7L, 7L, 7L, 7L, 10L, 10L, NA))
})

test_that("fwa_add_section_to_rm doesn't grab extra column", {
  x <- data.frame(BLK = 1L, RM = seq(1000L, 10000L, by = 1000L))
  y <- data.frame(BLK = 1L, RM = c(3000L, 7500L, 9000L), Section = c(3L, 7L, 10L),
                  extra = c("x", "y", "x"))

  x <- fwa_add_section_to_rm(x, y)
  expect_s3_class(x, "tbl")
  expect_identical(colnames(x), c("BLK", "RM", "Section"))
  expect_identical(x$BLK, rep(1L, 10))
  expect_identical(x$RM, seq(1000L, 10000L, by = 1000L))
  expect_identical(x$Section, c(3L, 3L, 3L, 7L, 7L, 7L, 7L, 10L, 10L, NA))
})
