test_that("test fwa_add_point_to_stream_measure", {
  watershed <- fwa_add_watershed_to_blk(data.frame(blk = 356308001, rm = 1000))
  network <- fwa_add_collection_to_polygon(watershed)
  network$blk <- network$blue_line_key
  streams <- fwa_join_stream_segments(network)
  points <- fwa_add_rms_to_blk(data.frame(blk = 356308001))

  x1 <- data.frame(blk = 356308001, stream_measure = 0)
  x1 <- fwa_add_point_to_stream_measure(x1, streams)
  expect_snapshot_data(x1, "add_point_x1")

  x2s <- data.frame(blk = c(356308001, 355992254), stream_measure = c(0, 1))
  x2s <- fwa_add_point_to_stream_measure(x2s, streams)
  expect_snapshot_data(x2s, "add_point_x2s")

  x0 <- data.frame(blk = 1, stream_measure = 0)
  expect_error(fwa_add_point_to_stream_measure(x0, streams), "must match")

  skip_on_os("linux")

  x2 <- data.frame(blk = 356308001, stream_measure = c(0, 100, 10000, 20000, 40000))
  x2 <- fwa_add_point_to_stream_measure(x2, streams)
  expect_snapshot_data(x2, "add_point_x2")

  x4 <- data.frame(blk = 356308001, stream_measure = c(0, 100, 10000, 20000, 40000))
  x4 <- fwa_add_point_to_stream_measure(x4, streams)
  expect_snapshot_data(x4, "add_point_x4")
})


test_that("example add point to stream measure", {
  skip_on_os("linux")
  watershed <- fwa_add_watershed_to_blk(data.frame(blk = 356308001, rm = 1000))
  network <- fwa_add_collection_to_polygon(watershed)
  network$blk <- network$blue_line_key
  streams <- fwa_join_stream_segments(network)
  points <- fwa_add_rms_to_blk(data.frame(blk = 356308001))
  points <- fwa_snap_stream_measure_to_point(points, streams)
  points <- points[c("blk", "stream_measure")]
  points <- fwa_add_point_to_stream_measure(points, streams)
  expect_snapshot_data(points, "apsm")
})
