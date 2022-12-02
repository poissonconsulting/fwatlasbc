test_that("fwa_snap_rms_to_rms works", {
  rlang::local_options(nocache = TRUE)

  rm <- fwa_add_rms_to_blk(data.frame(blk = 356308001))

  x <- rm[rm$rm %in% c(0, 2000, 5000, 6000, 7000),]
  rm <- rm[rm$rm %in% c(0, 2000, 5000, 6000, 7000, 10000),]
  x <- fwa_snap_rms_to_rms(x, rm)
  rm <- x$rm
  x <- x$x
  expect_s3_class(x, "sf")
  expect_identical(colnames(x), c("blk", "rm", "new_rm", "distance_to_new_rm", "elevation", "geometry"))
  expect_equal(x$blk, rep(356308001, 5))
  expect_equal(x$rm, c(0, 2000, 5000, 6000, 7000))
  expect_equal(x$new_rm, c(0, 2000, 5000, 6000, 7000))
  expect_equal(x$distance_to_new_rm, rep(0,5))
  expect_s3_class(x$geometry, "sfc_POINT")

  expect_s3_class(rm, "sf")
  expect_identical(colnames(rm), c("blk", "rm", "new_rm", "distance_to_new_rm", "elevation", "geometry"))
  expect_equal(rm$blk, rep(356308001, 6))
  expect_equal(rm$rm, c(0, 2000, 5000, 6000, 7000, 10000))
  expect_equal(rm$new_rm, c(0L, 2000L, 5000L, 6000L, 7000L, 7000L))
  expect_equal(rm$distance_to_new_rm, c(0, 0, 0, 0, 0, 1496.94367005726))
  expect_s3_class(rm$geometry, "sfc_POINT")
})

test_that("fwa_snap_rms_to_rms can deal with mising blks", {
  rlang::local_options(nocache = TRUE)

  rm <- fwa_add_rms_to_blk(data.frame(blk = 356308001))

  x <- rm[rm$rm %in% c(0, 2000, 5000, 6000, 7000),]
  rm <- rm[rm$rm %in% c(0, 2000, 5000, 6000, 7000, 10000),]
  rm$blk[1:2] <- 1
  x <- fwa_snap_rms_to_rms(x, rm)
  rm <- x$rm
  x <- x$x
  expect_s3_class(x, "sf")
  expect_identical(colnames(x), c("blk", "rm", "new_rm", "distance_to_new_rm", "elevation", "geometry"))
  expect_equal(x$blk, rep(356308001, 5))
  expect_equal(x$rm, c(0, 2000, 5000, 6000, 7000))
  expect_equal(x$new_rm, c(5000, 5000, 6000, 6000, 7000))
  expect_equal(x$distance_to_new_rm, c(3314.98934913696, 1837.8850050135, 772.26037010328, 0, 0))
  expect_s3_class(x$geometry, "sfc_POINT")

  expect_s3_class(rm, "sf")
  expect_identical(colnames(rm), c("blk", "rm", "new_rm", "distance_to_new_rm", "elevation", "geometry"))
  expect_equal(rm$blk, c(1,1,rep(356308001, 4)))
  expect_equal(rm$rm, c(0, 2000, 5000, 6000, 7000, 10000))
  expect_equal(rm$new_rm, c(NA, NA, 2000, 6000L, 7000L, 7000L))
  expect_equal(rm$distance_to_new_rm, c(NA, NA, 1837.8850050135, 0, 0, 1496.94367005726))
  expect_s3_class(rm$geometry, "sfc_POINT")
})
