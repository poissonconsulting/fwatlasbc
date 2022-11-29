test_that("fwa_snap_rm_to_rms works", {
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
