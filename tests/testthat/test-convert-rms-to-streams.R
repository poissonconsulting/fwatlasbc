test_that("convert_rms_to_streams works", {
  watershed <- fwa_add_watershed_to_blk(data.frame(blk = 356308001, rm = 1000))
  network <- fwa_add_collection_to_polygon(watershed)
  x <- fwa_convert_stream_network_to_rms(network, interval = 100)
  x <- fwa_convert_rms_to_streams(x)

  expect_s3_class(x$geometry, "sfc_LINESTRING")
  expect_identical(nrow(x), 39L)

  x <- sf::st_cast(x, "MULTIPOINT")
  expect_warning(x <- sf::st_cast(x, "POINT"))

  expect_s3_class(x$geometry, "sfc_POINT")
  expect_snapshot_data(x, "convert_rms_to_streams")
})
