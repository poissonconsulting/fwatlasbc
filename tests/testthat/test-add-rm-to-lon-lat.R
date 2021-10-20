test_that("fwa_add_rm_to_lon_lat works", {
  x <- fwa_add_rm_to_lon_lat(data.frame(lon = -132.26, lat = 53.36))
  expect_s3_class(x, "sf")
  expect_identical(colnames(x), c("lon", "lat", "blk", "rm", "distance_to_lon_lat", "geometry"
  ))
  expect_identical(x$lon, -132.26)
  expect_identical(x$lat, 53.36)
  expect_identical(x$blk, 360824839L)
  expect_equal(x$rm, 1118.34002309864)
  expect_equal(x$distance_to_lon_lat, 508.411631958987)
  expect_s3_class(x$geometry, "sfc_POINT")
})
