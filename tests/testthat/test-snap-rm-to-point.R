test_that("fwa_snap_rm_to_point works", {
  rm <- fwa_add_rms_to_blk(data.frame(blk = 356308001))

  x <- rm[rm$rm %in% c(0, 2000, 5000, 6000, 7000),]
  rm <- rm[rm$rm %in% c(1000, 3000, 4000, 8000, 9000, 10000),]
  x <- fwa_snap_rm_to_point(x, rm)
  expect_s3_class(x, "sf")
  expect_identical(colnames(x), c("blk", "rm", "distance_to_rm", "elevation", "geometry"))
  expect_equal(x$blk, rep(356308001, 5))
  expect_equal(x$rm, c(1000, 3000, 4000, 4000, 8000))
  expect_equal(x$distance_to_rm, c(873.50885850392, 535.63765010454, 754.230245890789, 610.731097004499,
                             514.952511361304))
  expect_s3_class(x$geometry, "sfc_POINT")
})

test_that("fwa_snap_rm_to_point works one row", {
  rm <- fwa_add_rms_to_blk(data.frame(blk = 356308001))

  x <- rm[rm$rm == 5000,]
  rm <- rm[rm$rm %in% c(1000, 3000, 4000, 8000, 9000, 10000),]
  x <- fwa_snap_rm_to_point(x, rm)
  expect_s3_class(x, "sf")
  expect_identical(colnames(x), c("blk", "rm", "distance_to_rm", "elevation", "geometry"))
  expect_equal(x$blk, 356308001)
  expect_equal(x$rm, 4000)
  expect_equal(x$distance_to_rm, 754.230245890789)
  expect_s3_class(x$geometry, "sfc_POINT")
})

test_that("fwa_snap_rm_to_point works", {
  rm <- fwa_add_rms_to_blk(data.frame(blk = 356308001))

  x <- rm[rm$rm %in% c(0, 2000, 5000, 6000, 7000),]
  rm <- rm[rm$rm %in% c(1000, 3000, 4000, 9000, 10000),]
  rm$blk[rm$rm == 3000] <- 2L
  x$blk[1:2] <- NA_integer_
  x <- fwa_snap_rm_to_point(x, rm)
  expect_s3_class(x, "sf")
  expect_identical(colnames(x), c("blk", "rm", "distance_to_rm", "elevation", "geometry"))
  expect_equal(x$blk, c(356308001, 2, rep(356308001, 3)))
  expect_equal(x$rm, c(1000, 3000, 4000, 4000, 9000))
  expect_equal(x$distance_to_rm, c(873.50885850392, 535.63765010454, 754.230245890789, 610.731097004499,
                             645.410842257104))
  expect_s3_class(x$geometry, "sfc_POINT")
})

test_that("fwa_snap_rm_to_point works active geometry geometry", {
  rm <- fwa_add_rms_to_blk(data.frame(blk = 356308001))

  x <- rm[rm$rm %in% c(0, 2000, 5000, 6000, 7000),]
  rm <- rm[rm$rm %in% c(1000, 3000, 4000, 9000, 10000),]
  rm$blk[rm$rm == 3000] <- 2L
  x$blk[1:2] <- NA_integer_
  x$geometry2 <- rm$geometry[1:5]
  x <- fwa_snap_rm_to_point(x, rm)
  expect_s3_class(x, "sf")
  expect_identical(colnames(x), c("blk", "rm", "distance_to_rm", "elevation", "geometry", "geometry2"))
  expect_equal(x$blk, c(356308001, 2, rep(356308001, 3)))
  expect_equal(x$rm, c(1000, 3000, 4000, 4000, 9000))
  expect_equal(x$distance_to_rm, c(873.50885850392, 535.63765010454, 754.230245890789, 610.731097004499,
                                   645.410842257104))
  expect_s3_class(x$geometry, "sfc_POINT")
})

test_that("fwa_snap_rm_to_point works active geometry geometry2", {
  rm <- fwa_add_rms_to_blk(data.frame(blk = 356308001))

  x <- rm[rm$rm %in% c(0, 2000, 5000, 6000, 7000),]
  rm <- rm[rm$rm %in% c(1000, 3000, 4000, 9000, 10000),]
  rm$blk[rm$rm == 3000] <- 2L
  x$blk[1:2] <- NA_integer_
  x$geometry2 <- rm$geometry[1:5]
  x <- sf::st_set_geometry(x, "geometry2")
  expect_identical(sf::st_geometry(x), x$geometry2)
  x <- fwa_snap_rm_to_point(x, rm)
  expect_identical(sf::st_geometry(x), x$geometry2)

  expect_s3_class(x, "sf")
  expect_identical(colnames(x), c("blk", "rm", "distance_to_rm", "elevation", "geometry", "geometry2"))
  expect_equal(x$blk, c(356308001, 2, rep(356308001, 3)))
  expect_equal(x$rm, c(1000, 3000, 4000, 9000, 10000))
  expect_equal(x$distance_to_rm, c(0, 0, 0, 0, 0))
  expect_s3_class(x$geometry, "sfc_POINT")
})
