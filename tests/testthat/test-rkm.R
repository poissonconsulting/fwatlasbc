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
  blk <- 356308001
  rkm <- fwa_rkm(blue_line_key = 356308001, interval = 1000)
  x <- rkm[rkm$rkm %in% c(0, 2, 5, 6, 7),]
  rkm <- rkm[!rkm$rkm %in% c(0, 2, 5, 6, 7),]
  rkm$blue_line_key[rkm$rkm == 8] <- 2L

  x2 <- x
  x2$blue_line_key[1:2] <- NA_integer_
  y <- fwa_nearest_rkm(x2, rkm)
  x$rkm <- c(1,3,4,4,9)
  expect_is(y$distance_to_rkm, "units")
  expect_true(vld_gt(as.numeric(y$distance_to_rkm), 500))
  y$distance_to_rkm <- NULL
  expect_identical(y, x)
})
