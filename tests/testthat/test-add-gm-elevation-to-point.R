test_that("fwa_add_gm_elevation_to_point works", {
  rm <- fwa_add_rms_to_blk(data.frame(blk = 356308001))

  x <- fwa_add_gm_elevation_to_point(rm)
  expect_s3_class(x, "sf")
  expect_identical(colnames(x), c("blk", "rm", "elevation", "geometry"))
  expect_equal(x$elevation,
                   c(5, 5, 5, 5, 5, 10.3940963745117, 5.35616397857666, 5.00019073486328,
                     9.28616428375244, 7.15765380859375, 7.00408458709717, 8.34540557861328,
                     10.8682670593262, 10.2141819000244, 11.2053289413452, 11.2773027420044,
                     10.5231266021728, 75.4190673828125, 165.339157104492, 208.487533569336
                   ))
})
