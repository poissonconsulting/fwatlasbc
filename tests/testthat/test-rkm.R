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
  ey <- x
  ey$rkm <- c(1,3,4,4,8)
  ey$distance_to_rkm <- c(873.50885850392, 535.63765010454, 754.230245890789, 610.731097004499,
                          514.952511361304)
  expect_equal(y, ey)

  rkm$blue_line_key[rkm$rkm == 8] <- 2L
  x2 <- x
  x2$blue_line_key[1:2] <- NA_integer_
  y <- fwa_nearest_rkm(x2, rkm)
  ey <- x
  ey$rkm <- c(1,3,4,4,9)
  ey$distance_to_rkm <- c(873.50885850392, 535.63765010454, 754.230245890789, 610.731097004499,
                          645.410842257104)
  expect_equal(y, ey[c("blue_line_key", "rkm", "distance_to_rkm")])

  x2 <- sf::st_zm(x2)
  is.na(x2$geometry[x2$rkm == 6]) <- TRUE
  y <- fwa_nearest_rkm(x2, rkm)
  ey <- sf::st_zm(x)
  is.na(ey$geometry[ey$rkm == 6]) <- TRUE
  ey$rkm <- c(1,3,4,NA,9)
  ey$distance_to_rkm <- c(873.50885850392, 535.63765010454, 754.230245890789, NA,
                          645.410842257104)
  y

  expect_equal(y, ey[c("blue_line_key", "rkm", "distance_to_rkm")])

  is.na(x2$blue_line_key) <- TRUE
  y <- fwa_nearest_rkm(x2, rkm)

  expect_identical(y$blue_line_key, c(356308001L, 356308001L, 356308001L, NA, 2L))
  expect_identical(y$rkm, c(1, 3, 4, NA, 8))
  expect_equal(y$distance_to_rkm, c(873.50885850392, 535.63765010454, 754.230245890789, NA,
                                      514.952511361304))
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
