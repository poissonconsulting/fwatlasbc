test_that("fwa_snap_rm_to_point works", {
  rm <- fwa_add_rms_to_blk(data.frame(BLK = 356308001))

  x <- rm[rm$RM %in% c(0, 2000, 5000, 6000, 7000),]
  rm <- rm[rm$RM %in% c(1000, 3000, 4000, 8000, 9000, 10000),]
  x <- fwa_snap_rm_to_point(x, rm)
  expect_s3_class(x, "sf")
  expect_identical(colnames(x), c("BLK", "RM", "Distance", "Elevation", "geometry"))
  expect_equal(x$BLK, rep(356308001, 5))
  expect_equal(x$RM, c(1000, 3000, 4000, 4000, 8000))
  expect_equal(x$Distance, c(873.50885850392, 535.63765010454, 754.230245890789, 610.731097004499,
                             514.952511361304))
  expect_s3_class(x$geometry, "sfc_POINT")
})

test_that("fwa_snap_rm_to_point works one row", {
  rm <- fwa_add_rms_to_blk(data.frame(BLK = 356308001))

  x <- rm[rm$RM == 5000,]
  rm <- rm[rm$RM %in% c(1000, 3000, 4000, 8000, 9000, 10000),]
  x <- fwa_snap_rm_to_point(x, rm)
  expect_s3_class(x, "sf")
  expect_identical(colnames(x), c("BLK", "RM", "Distance", "Elevation", "geometry"))
  expect_equal(x$BLK, 356308001)
  expect_equal(x$RM, 4000)
  expect_equal(x$Distance, 754.230245890789)
  expect_s3_class(x$geometry, "sfc_POINT")
})

test_that("fwa_snap_rm_to_point works", {
  rm <- fwa_add_rms_to_blk(data.frame(BLK = 356308001))

  x <- rm[rm$RM %in% c(0, 2000, 5000, 6000, 7000),]
  rm <- rm[rm$RM %in% c(1000, 3000, 4000, 9000, 10000),]
  rm$BLK[rm$RM == 3000] <- 2L
  x$BLK[1:2] <- NA_integer_
  x <- fwa_snap_rm_to_point(x, rm)
  expect_s3_class(x, "sf")
  expect_identical(colnames(x), c("BLK", "RM", "Distance", "Elevation", "geometry"))
  expect_equal(x$BLK, c(356308001, 2, rep(356308001, 3)))
  expect_equal(x$RM, c(1000, 3000, 4000, 4000, 9000))
  expect_equal(x$Distance, c(873.50885850392, 535.63765010454, 754.230245890789, 610.731097004499,
                             645.410842257104))
  expect_s3_class(x$geometry, "sfc_POINT")
})
