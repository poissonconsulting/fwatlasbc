test_that("fwa_convert_streams_to_rms", {
  watershed <- fwa_add_watershed_to_blk(data.frame(blk = 356308001, rm = 1000))
  network <- fwa_add_collection_to_polygon(watershed)
  streams <- dplyr::select(network, blk = blue_line_key)
  x <- fwa_convert_streams_to_rms(streams, interval = 100)
  expect_s3_class(x, "sf")
  # expect_identical(nrow(x), 862L)
  # expect_s3_class(x$geometry, "sfc_POINT")
  #
  # expect_snapshot_data(x, "fwa_convert_streams_to_rms")
  #
  # chk::expect_chk_error(fwa_convert_streams_to_rms(network, tolerance = 0.01))
})

test_that("fwa_convert_streams_to_rms elevation", {
  watershed <- fwa_add_watershed_to_blk(data.frame(blk = 356308001, rm = 1000))
  network <- fwa_add_collection_to_polygon(watershed)
  streams <- dplyr::select(network, blk = blue_line_key)
  x <- fwa_convert_streams_to_rms(streams, interval = 100, elevation = TRUE)
  expect_s3_class(x, "sf")
  # expect_identical(nrow(x), 862L)
  # expect_s3_class(x$geometry, "sfc_POINT")
  #
  # expect_snapshot_data(x, "fwa_convert_streams_to_rms")
  #
  # chk::expect_chk_error(fwa_convert_streams_to_rms(network, tolerance = 0.01))
})

test_that("fwa_convert_streams_to_rms", {
  watershed <- fwa_add_watershed_to_blk(data.frame(blk = 356308001, rm = 1000))
  network <- fwa_add_collection_to_polygon(watershed)

  channel <- network[network$linear_feature_id == 700730484,]
  channel$blue_line_key <- 156308001L
  channel$linear_feature_id <- 100730484L
  channel$stream_order <- 1L

  network <- rbind(network, channel)
  streams <- dplyr::select(network, blk = blue_line_key)

  x <- fwa_convert_streams_to_rms(streams, interval = 100)
  expect_s3_class(x, "sf")
  # expect_identical(nrow(x), 892L)
  # expect_s3_class(x$geometry, "sfc_POINT")
  #
  # expect_snapshot_data(x, "fwa_convert_to_rms_side")
  #
  # chk::expect_chk_error(fwa_convert_streams_to_rms(network, tolerance = 0.01))
})
