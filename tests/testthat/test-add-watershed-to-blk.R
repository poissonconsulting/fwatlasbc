test_that("fwa_add_watershed_to_blk works", {
  x <- fwa_add_watershed_to_blk(data.frame(blk = 360879896L))

  expect_s3_class(x, "sf")
  expect_identical(colnames(x), c("blk", "rm", "geometry"))
  expect_identical(x$blk, 360879896L)
  expect_identical(x$rm, 0)
  expect_s3_class(x$geometry, "sfc_POLYGON")
  expect_equal(as.numeric(sf::st_area(x)), 154167602.934787)
})

test_that("fwa_add_watershed_to_blk works 360883036 rm = 10", {
  x <- fwa_add_watershed_to_blk(data.frame(blk = 360883036, rm = 10))
  expect_s3_class(x, "sf")
  expect_identical(colnames(x), c("blk", "rm", "geometry"))
  expect_identical(x$rm, 10)
  expect_identical(x$blk, 360883036)
  expect_s3_class(x$geometry, "sfc_POLYGON")
  expect_equal(as.numeric(sf::st_area(x)), 36479050.6256187)
})

test_that("fwa_add_watershed_to_blk exclude excludes", {
  x <- fwa_add_watershed_to_blk(data.frame(blk = 360883036, rm = 10),
                                exclude = TRUE)
  expect_s3_class(x, "sf")
  expect_identical(colnames(x), c("blk", "rm", "geometry"))
  expect_identical(x$rm, 10)
  expect_identical(x$blk, 360883036)
  expect_s3_class(x$geometry, "sfc_POLYGON")
  expect_equal(as.numeric(sf::st_area(x)), 33609950.4141863)
})

test_that("fwa_add_watershed_to_blk doesn't work when rm = 0 for 360883036 even with exclude", {
  chk::expect_chk_error(fwa_add_watershed_to_blk(data.frame(blk = 360883036), exclude = TRUE),
                   "Watershed undefined for blk 360883036 at rm 0 \\(try changing the rm\\)\\.$")
})
