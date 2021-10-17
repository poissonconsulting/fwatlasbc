test_that("fwa_nearest_rm works", {
  rm <- fwa_add_rms_to_blk(data.frame(BLK = 356308001))

  x <- rm[rm$RM %in% c(0, 2000, 5000, 6000, 7000),]
  rm <- rm[rm$RM %in% c(1000, 3000, 4000, 8000, 9000, 10000),]
  x <- fwa_snap_rm_to_point(x, rm)
  expect_s3_class(x, "sf")
  expect_identical(colnames(x), c("BLK", "RM", "geometry", "Distance"))
  expect_equal(x$BLK, rep(356308001, 5))
  expect_equal(x$RM, c(1000, 3000, 4000, 4000, 8000))
  expect_equal(x$Distance, c(873.50885850392, 535.63765010454, 754.230245890789, 610.731097004499,
                             514.952511361304))
  expect_s3_class(x$geometry, "sfc_POINT")
})

test_that("fwa_nearest_rm works", {
  rm <- fwa_add_rms_to_blk(data.frame(BLK = 356308001))

  x <- rm[rm$RM %in% c(0, 2000, 5000, 6000, 7000),]
  rm <- rm[rm$RM %in% c(1000, 3000, 4000, 8000, 9000, 10000),]
  rm$BLK[rm$RM == 8000] <- 2L
  x$BLK[1:2] <- NA_integer_
  x <- fwa_snap_rm_to_point(x, rm)
  expect_s3_class(x, "sf")
  expect_identical(colnames(x), c("BLK", "RM", "geometry", "Distance"))
  expect_equal(x$BLK, rep(356308001, 5))
  expect_equal(x$RM, c(1000, 3000, 4000, 4000, 9000))
  expect_equal(x$Distance, c(873.50885850392, 535.63765010454, 754.230245890789, 610.731097004499,
                             645.410842257104))
  expect_s3_class(x$geometry, "sfc_POINT")
})

  x2 <- sf::st_zm(x2)
  is.na(x2$geometry[x2$rm == 6000]) <- TRUE
  y <- fwa_nearest_rm(x2, rm)
  ey <- sf::st_zm(x)
  is.na(ey$geometry[ey$rm == 6000]) <- TRUE
  ey$rm <- c(1000,3000,4000,NA,9000)
  ey$distance_to_rm <- c(873.50885850392, 535.63765010454, 754.230245890789, NA,
                         645.410842257104)
  y

  expect_equal(dplyr::as_tibble(y), dplyr::as_tibble(ey),
               ignore_attr = TRUE)

  is.na(x2$blue_line_key) <- TRUE
  y <- fwa_nearest_rm(x2, rm)

  expect_identical(y$blue_line_key, c(356308001L, 356308001L, 356308001L, NA, 2L))
  expect_identical(y$rm, c(1000L, 3000L, 4000L, NA, 8000L))
  expect_equal(y$distance_to_rm, c(873.50885850392, 535.63765010454, 754.230245890789, NA,
                                   514.952511361304))
})
