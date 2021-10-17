test_that("fwa_add_watershed_to_blk works", {
  x <- fwa_add_watershed_to_blk(data.frame(BLK = 356308001, extra_col = "ex"),
                                          start = 10)

  expect_s3_class(x, "sf")
  expect_identical(colnames(x), c("BLK", "extra_col", "StartRM", "Area", "geometry"))
  expect_identical(x$BLK, 356308001)
  expect_identical(x$extra_col, "ex")
  expect_identical(x$StartRM, 10)
  expect_s3_class(x$geometry, "sfc_POLYGON")
})
