test_that("fwa_snap_rm_to_rms works", {
  rlang::local_options(nocache = TRUE)

  rm <- fwa_add_rms_to_blk(data.frame(blk = 356308001))

  x <- rm[rm$rm %in% c(0, 2000, 5000, 6000, 7000),]
  rm <- rm[rm$rm %in% c(1000, 3000, 4000, 8000, 9000, 10000),]
  x <- fwa_snap_rm_to_rms(x, rm)
  expect_s3_class(x, "sf")
  expect_identical(colnames(x), c("blk", "rm", "new_rm", "distance_to_new_rm", "elevation", "geometry"))
  expect_equal(x$blk, rep(356308001, 5))
  expect_equal(x$rm, c(0, 2000, 5000, 6000, 7000))
  expect_equal(x$new_rm, c(1000, 3000, 4000, 4000, 8000))
  expect_equal(x$distance_to_new_rm, c(873.50885850392, 535.63765010454, 754.230245890789, 610.731097004499,
                                   514.952511361304))
  expect_s3_class(x$geometry, "sfc_POINT")
})

test_that("fwa_snap_rm_to_rms does snap mouth", {
  rm <- fwa_add_rms_to_blk(data.frame(blk = 356308001), nocache = FALSE)

  x <- rm[rm$rm %in% c(0, 3000, 6000),]
  rm <- rm[rm$rm %in% c(3000, 6000, 9000),]
  rm$rm <- c(3000, 6000, 0)

  x <- fwa_snap_rm_to_rms(x, rm, snap_zeros = TRUE)

  expect_s3_class(x, "sf")
  expect_identical(colnames(x), c("blk", "rm", "new_rm", "distance_to_new_rm", "elevation", "geometry"))
  expect_equal(x$blk, rep(356308001, 3))
  expect_equal(x$rm, c(0, 3000, 6000))
  expect_equal(x$new_rm, c(0, 3000, 6000))
  expect_equal(x$distance_to_new_rm, c(2331.96951067872, 0, 0))
  expect_s3_class(x$geometry, "sfc_POINT")
})

test_that("fwa_snap_rm_to_rms doesn't snap mouth", {
  rm <- fwa_add_rms_to_blk(data.frame(blk = 356308001), nocache = FALSE)

  x <- rm[rm$rm %in% c(0, 3000, 6000),]
  rm <- rm[rm$rm %in% c(3000, 6000, 9000),]
  rm$rm <- c(3000, 6000, 0)

  x <- fwa_snap_rm_to_rms(x, rm, snap_zeros = FALSE)

  expect_s3_class(x, "sf")
  expect_identical(colnames(x), c("blk", "rm", "new_rm", "distance_to_new_rm", "elevation", "geometry"))
  expect_equal(x$blk, rep(356308001, 3))
  expect_equal(x$rm, c(0, 3000, 6000))
  expect_equal(x$new_rm, c(3000, 3000, 6000))
  skip("update distance to new rm")
  expect_equal(x$distance_to_new_rm, c(0, 0, 0))
  expect_s3_class(x$geometry, "sfc_POINT")
})

test_that("fwa_snap_rm_to_rms no x", {
  rm <- fwa_add_rms_to_blk(data.frame(blk = 356308001), nocache = FALSE)

  x <- rm[FALSE,]
  rm <- rm[rm$rm %in% c(3000, 6000, 9000),]
  rm$rm <- c(3000, 6000, 0)

  x <- fwa_snap_rm_to_rms(x, rm, snap_zeros = FALSE)

  expect_s3_class(x, "sf")
  expect_identical(colnames(x), c("blk", "rm", "new_rm", "distance_to_new_rm", "elevation", "geometry"))
  expect_equal(x$blk, integer(0))
  expect_equal(x$rm, numeric(0))
  expect_equal(x$new_rm, integer(0))
  expect_equal(x$distance_to_new_rm, numeric(0))
  expect_s3_class(x$geometry, "sfc_GEOMETRY")
})

test_that("fwa_snap_rm_to_rms no rm", {
  rm <- fwa_add_rms_to_blk(data.frame(blk = 356308001), nocache = FALSE)

  x <- rm[rm$rm %in% c(0, 3000, 6000),]
  rm <- rm[FALSE,]
  x <- fwa_snap_rm_to_rms(x, rm, snap_zeros = FALSE)

  expect_s3_class(x, "sf")
  expect_identical(colnames(x), c("blk", "rm", "new_rm", "distance_to_new_rm", "elevation", "geometry"))
  expect_equal(x$blk, rep(356308001, 3))
  expect_equal(x$rm, c(0, 3000, 6000))
  expect_equal(x$new_rm, rep(NA_integer_, 3))
  expect_equal(x$distance_to_new_rm, rep(NA_real_, 3))
  expect_s3_class(x$geometry, "sfc_POINT")
})

test_that("fwa_snap_rm_to_rms no x or rm", {
  rm <- fwa_add_rms_to_blk(data.frame(blk = 356308001), nocache = FALSE)

  x <- rm[FALSE,]
  rm <- rm[FALSE,]

  x <- fwa_snap_rm_to_rms(x, rm, snap_zeros = FALSE)

  expect_s3_class(x, "sf")
  expect_identical(colnames(x), c("blk", "rm", "new_rm", "distance_to_new_rm", "elevation", "geometry"))
  expect_equal(x$blk, integer(0))
  expect_equal(x$rm, numeric(0))
  expect_equal(x$new_rm, integer(0))
  expect_equal(x$distance_to_new_rm, numeric(0))
  expect_s3_class(x$geometry, "sfc_GEOMETRY")
})

test_that("fwa_snap_rm_to_rms new_rm preserves all new_rm", {
  rm <- fwa_add_rms_to_blk(data.frame(blk = 356308001), nocache = FALSE)

  x <- rm[rm$rm %in% c(0, 3000, 6000),]
  rm <- rm[rm$rm %in% c(3000, 6000, 9000),]
  rm$rm <- c(3000, 6000, 0)
  x$new_rm <- c(3000, 6000, 9000)

  x <- fwa_snap_rm_to_rms(x, rm)
  expect_s3_class(x, "sf")
  expect_identical(colnames(x), c("blk", "rm", "new_rm", "distance_to_new_rm", "elevation", "geometry"))
  expect_equal(x$blk, rep(356308001, 3))
  expect_equal(x$rm, c(0, 3000, 6000))
  expect_equal(x$new_rm, c(3000, 6000, 9000))
  skip("distance update at end")
  expect_equal(x$distance_to_new_rm, rep(NA_real_, 3))
  expect_s3_class(x$geometry, "sfc_POINT")
})

test_that("fwa_snap_rm_to_rms new_rm errors if not sorted", {
  rm <- fwa_add_rms_to_blk(data.frame(blk = 356308001), nocache = FALSE)

  x <- rm[rm$rm %in% c(0, 3000, 6000),]
  rm <- rm[rm$rm %in% c(3000, 6000, 9000),]
  rm$rm <- c(3000, 6000, 0)
  x$new_rm <- c(3000, 6000, 0)

  expect_error(fwa_snap_rm_to_rms(x, rm), "`x\\$new_rm` must be sorted\\.$")
})


