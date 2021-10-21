test_that("fwa_convert_stream_network_to_rms", {
  watershed <- fwa_add_watershed_to_blk(data.frame(blk = 356308001, rm = 1000))
  stream_network <- fwa_add_collection_to_watershed(watershed)
  chk::expect_chk_error(fwa_convert_stream_network_to_rms(stream_network, tolerance = 0.01))
  x <- fwa_convert_stream_network_to_rms(stream_network, interval = 100)
})
