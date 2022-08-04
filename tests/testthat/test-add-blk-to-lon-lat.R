test_that("fwa_add_blk_to_lon_lat works", {
  x <- fwa_add_blk_to_lon_lat(data.frame(lon = -132.26, lat = 53.36))
  expect_s3_class(x, "sf")
  expect_identical(colnames(x), c("lon", "lat", "blk", "rm", "distance_to_lon_lat", "geometry"
  ))
  expect_identical(x$lon, -132.26)
  expect_identical(x$lat, 53.36)
  expect_identical(x$blk, 360824839)
  expect_equal(x$rm, 1118.34002309864)
  expect_equal(x$distance_to_lon_lat, 508.412)
  expect_s3_class(x$geometry, "sfc_POINT")
})

test_that("fwa_add_blk_to_lon_lat works multiple ", {
  x <- fwa_add_blk_to_lon_lat(data.frame(lon = c(-132.26, -132.25), lat = 53.36), limit = 3)
  expect_s3_class(x, "sf")
  expect_identical(colnames(x), c("lon", "lat", "blk", "rm", "distance_to_lon_lat", "geometry"
  ))
  expect_identical(nrow(x), 6L)
  expect_snapshot_data(x, "multiple")
})

test_that("fwa_add_blk_to_lon_lat works none ", {
  x <- fwa_add_blk_to_lon_lat(data.frame(lon = -132.26, lat = 53.36), tolerance = 500)
  expect_s3_class(x, "sf")
  expect_identical(colnames(x), c("lon", "lat", "blk", "rm", "distance_to_lon_lat", "geometry"
  ))
  expect_identical(x$lon, numeric(0))
  expect_identical(x$lat, numeric(0))
  expect_identical(x$blk, integer(0))
  expect_equal(x$rm, numeric(0))
  expect_equal(x$distance_to_lon_lat, numeric(0))
  expect_s3_class(x$geometry, "sfc_GEOMETRY")
})
