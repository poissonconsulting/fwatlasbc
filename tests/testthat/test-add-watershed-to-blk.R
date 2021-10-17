test_that("fwa_add_watershed_to_blk works", {
  x <- fwa_add_watershed_to_blk(data.frame(BLK = 360879896L))

  expect_s3_class(x, "sf")
  expect_identical(colnames(x), c("BLK", "geometry"))
  expect_identical(x$BLK, 360879896L)
  expect_s3_class(x$geometry, "sfc_POLYGON")
  expect_equal(as.numeric(sf::st_area(x)), 154167602.934787)
})

test_that("fwa_add_watershed_to_blk", {
  x <- fwa_add_watershed_to_blk(data.frame(BLK = 360879896L), include_fundamental = FALSE)

  expect_s3_class(x, "sf")
  expect_identical(colnames(x), c("BLK", "geometry"))
  expect_identical(x$BLK, 360879896L)
  expect_s3_class(x$geometry, "sfc_POLYGON")
  expect_equal(as.numeric(sf::st_area(x)), 154121020.15179)
})
