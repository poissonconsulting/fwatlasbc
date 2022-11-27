test_that("fwa_add_rms_to_blk works simple case", {
  rlang::local_options(nocache = TRUE)

  x <- fwa_add_rms_to_blk(data.frame(blk = 356308001, extra_col = "extra"),
                          interval = 100, start = 10001)
  expect_s3_class(x, "sf")
  expect_identical(names(x), c("blk", "extra_col", "rm", "elevation", "geometry"))
  expect_identical(x$blk, rep(356308001, 93))
  expect_identical(x$extra_col, rep("extra", 93))
  expect_equal(x$rm, seq(10001L, 19201L, by = 100L))
  expect_equal(x$elevation[1], 10.5)
  expect_s3_class(x$geometry, "sfc_POINT")
  expect_identical(colnames(sf::st_coordinates(x)), c("X", "Y"))
})

test_that("fwa_add_rms_to_blk works multiple", {
  rlang::local_options(nocache = TRUE)

  x <- fwa_add_rms_to_blk(data.frame(blk = c(356308001, 354091024)))
  expect_s3_class(x, "sf")
  expect_identical(names(x), c("blk", "rm", "elevation", "geometry"))
  expect_identical(x$blk, c(rep(356308001, 20), rep(354091024, 3)))
  expect_equal(x$rm, c(seq(0, 19000, by = 1000), c(0, 1000, 2000)))
  expect_s3_class(x$geometry, "sfc_POINT")
  expect_identical(colnames(sf::st_coordinates(x)), c("X", "Y"))
})

test_that("fwa_add_rms_to_blk errors missing blk", {
  rlang::local_options(nocache = TRUE)

  expect_error(fwa_add_rms_to_blk(data.frame(blk = NA_integer_)))
})

test_that("fwa_add_rms_to_blk errors no blk", {
  rlang::local_options(nocache = TRUE)

  expect_error(fwa_add_rms_to_blk(blk = integer(0)))
})

test_that("fwa_add_rms_to_blk errors not recognised", {
  rlang::local_options(nocache = TRUE)

  expect_error(fwa_add_rms_to_blk(data.frame(blk = 1L)))
})
