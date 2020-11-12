test_that("rkm functions work", {

  blk <- 356308001
  drm <- 0
  drm2 <- 10000

  x <- fwa_rkm(blue_line_key = blk, interval = 100, distance_upstream = drm2)
  expect_is(x, "sf")
  expect_identical(names(x), c("blue_line_key", "rkm", "geometry"))
  expect_is(x$blue_line_key, "integer")
})

test_that("fwa_nearest_rkm works", {
  rkm <- fwa_rkm(blue_line_key = 356308001, interval = 1000)

  x <- rkm[rkm$rkm %in% c(0, 2, 5, 6, 7),]
  rkm <- rkm[!rkm$rkm %in% c(0, 2, 5, 6, 7),]
  y <- fwa_nearest_rkm(x, rkm)
  expect_is(y$distance_to_rkm, "numeric")
  expect_true(vld_gt(as.numeric(y$distance_to_rkm), 500))
  y$distance_to_rkm <- NULL
  x$rkm <- c(1,3,4,4,8)
  expect_identical(y, x)


  rkm$blue_line_key[rkm$rkm == 8] <- 2L
  x2 <- x
  x2$blue_line_key[1:2] <- NA_integer_
  y <- fwa_nearest_rkm(x2, rkm)
  expect_is(y$distance_to_rkm, "numeric")
  expect_true(vld_gt(as.numeric(y$distance_to_rkm), 500))
  y$distance_to_rkm <- NULL
  x$rkm <- c(1,3,4,4,9)
  expect_identical(y, x)
})

test_that("fwa_add_columns_to_rkm adds no columns", {

  rkm <- data.frame(blue_line_key = 1L, rkm = seq(1, 10, by = 1))
  x <- data.frame(blue_line_key = 1L, rkm = c(3, 7.5, 9))

  expect_identical(fwa_add_columns_to_rkm(rkm, x), rkm)
})

test_that("fwa_add_columns_to_rkm reorders", {
  rkm <- data.frame(blue_line_key = 1L, rkm = seq(1, 10, by = 1))
  x <- data.frame(blue_line_key = 1L, rkm = c(3, 7.5, 9))

  erkm <- rkm
  rkm <- rkm[rev(order(rkm$rkm)),]

  expect_identical(fwa_add_columns_to_rkm(rkm, x), erkm)
})

test_that("fwa_add_columns_to_rkm adds missing values if zero length", {
  rkm <- data.frame(blue_line_key = 1L, rkm = seq(1, 10, by = 1))
  x <- data.frame(blue_line_key = 1L, rkm = c(3, 7.5, 9), new = 2)
  rkm$new <- NA_real_
  expect_identical(fwa_add_columns_to_rkm(rkm, x[0,]), rkm)
})

test_that("fwa_add_columns_to_rkm adds zero length", {
  rkm <- data.frame(blue_line_key = 1L, rkm = seq(1, 10, by = 1))
  x <- data.frame(blue_line_key = 1L, rkm = c(3, 7.5, 9), new = 2)

  erkm <- rkm[0,]
  erkm$new <- erkm$new <- numeric(0)
  expect_identical(fwa_add_columns_to_rkm(rkm[0,], x), erkm)
})

test_that("fwa_add_columns_to_rkm simple example", {
  rkm <- data.frame(blue_line_key = 1L, rkm = seq(1, 10, by = 1))
  y <- data.frame(blue_line_key = 1L, rkm = c(3, 7.5, 9), new = c(3, 7.5, 10))

  erkm <- rkm
  erkm$new <- c(3, 3, 3, 7.5, 7.5, 7.5, 7.5, 10, 10, NA)
  expect_identical(fwa_add_columns_to_rkm(rkm, y), erkm)
})

test_that("fwa_add_columns_to_rkm with missing values", {
  rkm <- data.frame(blue_line_key = 1L, rkm = seq(1, 10, by = 1))
  y <- data.frame(blue_line_key = 1L, rkm = c(2, 3, 7.5, 9), new = c(NA, 3, 7.5, 10))

  erkm <- rkm
  erkm$new <- c(NA, NA, 3, 7.5, 7.5, 7.5, 7.5, 10, 10, NA)
  expect_identical(fwa_add_columns_to_rkm(rkm, y), erkm)
})

test_that("fwa_add_columns_to_rkm with more than one column", {
  rkm <- data.frame(blue_line_key = 1L, rkm = seq(1, 10, by = 1))
  y <- data.frame(blue_line_key = 1L, rkm = c(2, 3, 7.5, 9), new2 = c(10, 7.5, 3, NA),
                  new = c(NA, 3, 7.5, 10))

  erkm <- rkm
  erkm$new2 <- c(10, 10, 7.5, 3, 3, 3, 3, NA, NA, NA)
  erkm$new <- c(NA, NA, 3, 7.5, 7.5, 7.5, 7.5, 10, 10, NA)
  expect_identical(fwa_add_columns_to_rkm(rkm, y), erkm)
})
