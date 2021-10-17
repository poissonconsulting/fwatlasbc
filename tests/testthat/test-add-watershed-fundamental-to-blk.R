test_that("fwa_add_watershed_fundamental_to_blk works", {
  x <- fwa_add_watershed_fundamental_to_blk(data.frame(BLK = 360879896L, extra_col = "ex"))

  expect_s3_class(x, "sf")
  expect_identical(colnames(x), c("BLK", "extra_col", "geometry"))
  expect_identical(x$BLK, 360879896L)
  expect_identical(x$extra_col, "ex")
  expect_s3_class(x$geometry, "sfc_POLYGON")
  expect_equal(as.numeric(sf::st_area(x)), 46582.782996475)
})
