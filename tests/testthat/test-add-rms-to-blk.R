test_that("fwa_add_rms_to_blk works simple case", {
  x <- fwa_add_rms_to_blk(data.frame(BLK = 356308001, extra_col = "extra"),
                          interval = 100, start = 10001)
  expect_s3_class(x, "sf")
  expect_identical(names(x), c("BLK", "extra_col", "rm", "Elevation", "geometry"))
  expect_identical(x$BLK, rep(356308001, 93))
  expect_identical(x$extra_col, rep("extra", 93))
  expect_equal(x$rm, seq(10001L, 19201L, by = 100L))
  expect_equal(x$Elevation[1], 10.5)
  expect_s3_class(x$geometry, "sfc_POINT")
  expect_identical(colnames(sf::st_coordinates(x)), c("X", "Y"))
})

test_that("fwa_add_rms_to_blk works multiple", {
  x <- fwa_add_rms_to_blk(data.frame(BLK = c(356308001, 354091024)))
  expect_s3_class(x, "sf")
  expect_identical(names(x), c("BLK", "rm", "Elevation", "geometry"))
  expect_identical(x$BLK, c(rep(356308001, 20), rep(354091024, 3)))
  expect_equal(x$rm, c(seq(0, 19000, by = 1000), c(0, 1000, 2000)))
  expect_s3_class(x$geometry, "sfc_POINT")
  expect_identical(colnames(sf::st_coordinates(x)), c("X", "Y"))
})

test_that("fwa_add_rms_to_blk errors missing blk", {
  expect_error(fwa_add_rms_to_blk(data.frame(BLK = NA_integer_)))
})

test_that("fwa_add_rms_to_blk errors no blk", {
  expect_error(fwa_add_rms_to_blk(BLK = integer(0)))
})

test_that("fwa_add_rms_to_blk errors not recognised", {
  expect_error(fwa_add_rms_to_blk(data.frame(BLK = 1L)))
})
