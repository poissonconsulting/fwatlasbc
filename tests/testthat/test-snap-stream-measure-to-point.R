test_that("fwa_snap_stream_measure_to_point", {
  watershed <- fwa_add_watershed_to_blk(data.frame(blk = 356308001, rm = 1000))
  network <- fwa_add_collection_to_polygon(watershed)
  streams <- dplyr::select(network, blk = blue_line_key) |>
    dplyr::filter(blk %in% c("355992255", "356308001")) |>
    fwa_join_stream_segments()
  points <- fwa_convert_streams_to_rms(streams, interval = 500) |>
    dplyr::select(blk, old_rm = rm)

  pointsFT <- fwa_snap_stream_measure_to_point(points[FALSE,], streams)
  expect_identical(nrow(pointsFT), 0L)
  expect_snapshot_data(pointsFT, "pointsFT")
  pointsTF <- fwa_snap_stream_measure_to_point(points, streams[FALSE,])
  expect_identical(nrow(pointsTF), nrow(points))
  expect_snapshot_data(pointsTF, "pointsTF")
  pointsFF <- fwa_snap_stream_measure_to_point(points[FALSE,], streams[FALSE,])
  expect_identical(nrow(pointsFF), 0L)
  expect_snapshot_data(pointsFF, "pointsFF")

  points$extra <- 1:nrow(points)

  points_no_blk <- points |> dplyr::select(!blk)
  pointsFT_no_blk <- fwa_snap_stream_measure_to_point(points_no_blk[FALSE,], streams)
  expect_identical(nrow(pointsFT_no_blk), 0L)
  expect_snapshot_data(pointsFT_no_blk, "pointsFT_no_blk")
  pointsTF_no_blk <- fwa_snap_stream_measure_to_point(points_no_blk, streams[FALSE,])
  expect_identical(nrow(pointsTF_no_blk), nrow(points))
  expect_snapshot_data(pointsTF_no_blk, "pointsTF_no_blk")
  pointsFF_no_blk <- fwa_snap_stream_measure_to_point(points_no_blk[FALSE,], streams[FALSE,])
  expect_identical(nrow(pointsFF_no_blk), 0L)
  expect_snapshot_data(pointsFF_no_blk, "pointsFF_no_blk")

  stream_measure <- fwa_snap_stream_measure_to_point(points, streams)
  expect_identical(nrow(stream_measure), nrow(points))
  expect_snapshot_data(stream_measure, "stream_measure")

  stream_measure_no_blk <- fwa_snap_stream_measure_to_point(points_no_blk, streams)
  expect_identical(nrow(stream_measure_no_blk), nrow(points))
  expect_snapshot_data(stream_measure_no_blk, "stream_measure_no_blk")
  expect_identical(stream_measure_no_blk$blk, stream_measure$blk)

  points_no_blk_off <- points_no_blk
  points_no_blk_off$geometry <- st_set_crs(points_no_blk_off$geometry - 5000, st_crs(points))

  stream_measure_no_blk_off <- fwa_snap_stream_measure_to_point(points_no_blk_off, streams)
  expect_identical(nrow(stream_measure_no_blk_off), nrow(points))
  expect_snapshot_data(stream_measure_no_blk_off, "stream_measure_no_blk_off")

  points_blk_diff <- points
  points_blk_diff$blk[points_blk_diff$blk == 355992255] <- 356308001

  stream_measure_blk_diff <- fwa_snap_stream_measure_to_point(points_blk_diff, streams)
  expect_identical(nrow(stream_measure_blk_diff), nrow(points))
  expect_snapshot_data(stream_measure_blk_diff, "stream_measure_blk_diff")
})
