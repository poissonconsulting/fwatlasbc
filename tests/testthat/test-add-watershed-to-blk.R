test_that("fwa_add_watershed_to_blk works", {
  rlang::local_options(nocache = TRUE)

  x <- fwa_add_watershed_to_blk(data.frame(blk = 360879896L))

  expect_s3_class(x, "sf")
  expect_identical(colnames(x), c("blk", "rm", "geometry"))
  expect_identical(x$blk, 360879896L)
  expect_identical(x$rm, 0)
  expect_s3_class(x$geometry, "sfc_POLYGON")
  expect_equal(as.numeric(sf::st_area(x)), 154167602.934787)
})

test_that("fwa_add_watershed_to_blk works 360883036 rm = 10", {
  rlang::local_options(nocache = TRUE)

  x <- fwa_add_watershed_to_blk(data.frame(blk = 360883036, rm = 10))
  expect_s3_class(x, "sf")
  expect_identical(colnames(x), c("blk", "rm", "geometry"))
  expect_identical(x$rm, 10)
  expect_identical(x$blk, 360883036)
  expect_s3_class(x$geometry, "sfc_POLYGON")
  expect_equal(as.numeric(sf::st_area(x)), 36479050.6256187)
})

test_that("fwa_add_watershed_to_blk exclude excludes", {
  rlang::local_options(nocache = TRUE)

  x <- fwa_add_watershed_to_blk(
    data.frame(blk = 360883036, rm = 10),
    exclude = TRUE
  )
  expect_s3_class(x, "sf")
  expect_identical(colnames(x), c("blk", "rm", "geometry"))
  expect_identical(x$rm, 10)
  expect_identical(x$blk, 360883036)
  expect_s3_class(x$geometry, "sfc_POLYGON")
  expect_equal(as.numeric(sf::st_area(x)), 33609950.4141863)
})

test_that("fwa_add_watershed_to_blk doesn't work when rm = 0 for 360883036 even with exclude", {
  rlang::local_options(nocache = TRUE)

  chk::expect_chk_error(fwa_add_watershed_to_blk(
    data.frame(blk = 360883036),
    exclude = TRUE
  ))
})

test_that("fwa_add_watershed_to_blk errors when no watershed returned", {
  local_mocked_bindings(
    fwa_watershed_at_measure = function(...) {
      sf::st_sf(geometry = sf::st_sfc(crs = 3005))
    }
  )
  expect_snapshot(
    fwa_add_watershed_to_blk(data.frame(blk = 356308001, rm = 20000)),
    error = TRUE
  )
})

test_that("fwa_add_watershed_to_blk errors when rm beyond end of stream", {
  rlang::local_options(nocache = TRUE)
  expect_error(
    fwa_add_watershed_to_blk(data.frame(blk = 356308001, rm = 20000)),
    "No watershed was returned for blk 356308001 at rm 20000"
  )
})
