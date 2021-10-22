test_that("fwa_convert_stream_network_to_rms", {
  watershed <- fwa_add_watershed_to_blk(data.frame(blk = 356308001, rm = 1000))
  network <- fwa_add_collection_to_watershed(watershed)
  x <- fwa_convert_stream_network_to_rms(network, interval = 100)
  expect_s3_class(x, "sf")
  expect_identical(nrow(x), 861L)
  expect_s3_class(x$geometry, "sfc_POINT")

  expect_snapshot_data(x, "fwa_convert_stream_network_to_rms")

  chk::expect_chk_error(fwa_convert_stream_network_to_rms(network, tolerance = 0.01))
})
