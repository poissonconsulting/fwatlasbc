test_that("fwa_snap_stream_measure_to_point", {
  watershed <- fwa_add_watershed_to_blk(data.frame(blk = 356308001, rm = 1000))
  network <- fwa_add_collection_to_polygon(watershed)
  streams <- dplyr::select(network, blk = blue_line_key) |>
    dplyr::filter(blk %in% c("355992255", "356308001")) |>
    fwa_join_stream_segments()
  points <- fwa_convert_streams_to_rms(streams, interval = 500) |>
    dplyr::select(blk, old_rm = rm)


  pointsFT <- fwa_snap_stream_measure_to_point(points[FALSE,], streams)
  expect_snapshot_data(pointsFT, "pointsFT")
  pointsTF <- fwa_snap_stream_measure_to_point(points, streams[FALSE,])
  expect_snapshot_data(pointsFT, "pointsTF")
  pointsFF <- fwa_snap_stream_measure_to_point(points[FALSE,], streams[FALSE,])
  expect_snapshot_data(pointsFT, "pointsFF")

  points_no_blk <- points |> dplyr::select(!blk)
  pointsFT_no_blk <- fwa_snap_stream_measure_to_point(points_no_blk[FALSE,], streams)
  expect_snapshot_data(pointsFT, "pointsFT_no_blk")
  pointsTF_no_blk <- fwa_snap_stream_measure_to_point(points_no_blk, streams[FALSE,])
  expect_snapshot_data(pointsFT, "pointsTF_no_blk")
  pointsFF_no_blk <- fwa_snap_stream_measure_to_point(points_no_blk[FALSE,], streams[FALSE,])
  expect_snapshot_data(pointsFT, "pointsFF_no_blk")

  new <- fwa_snap_stream_measure_to_point(points, streams)
})
