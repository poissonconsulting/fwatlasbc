test_that("example add point to stream measure", {
  watershed <- fwa_add_watershed_to_blk(data.frame(blk = 356308001, rm = 1000))
  network <- fwa_add_collection_to_polygon(watershed)
  network$blk <- network$blue_line_key
  streams <- fwa_join_stream_segments(network)
  points <- fwa_add_rms_to_blk(data.frame(blk = 356308001))
  points <- fwa_snap_stream_measure_to_point(points, streams)
  points <- points[c("blk", "stream_measure")]
  fwa_add_point_to_stream_measure(points, streams)
  expect_snapshot_data(points, "fwa_add_point_to_stream_measure_example")
})
