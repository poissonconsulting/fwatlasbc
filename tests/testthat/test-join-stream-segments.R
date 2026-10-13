test_that("fwa_join_stream_segments", {
  watershed <- fwa_add_watershed_to_blk(data.frame(blk = 356308001, rm = 1000))
  network <- fwa_add_collection_to_polygon(watershed)
  streams <- dplyr::select(network, blk = blue_line_key) |>
    dplyr::filter(blk %in% c("355992255", "356308001"))
  x <- fwa_join_stream_segments(streams)
  expect_s3_class(x, "sf")
  expect_identical(nrow(x), 2L)
  expect_s3_class(x$geometry, "sfc_LINESTRING")

  expect_snapshot_data(x, "fwa_join_stream_segments")
})

test_that("fwa_join_stream_segments elevation", {
  skip_on_runiverse()

  watershed <- fwa_add_watershed_to_blk(data.frame(blk = 356308001, rm = 1000))
  network <- fwa_add_collection_to_polygon(watershed)
  streams <- dplyr::select(network, blk = blue_line_key)
  x <- fwa_join_stream_segments(streams, elevation = TRUE, source = "google")
  expect_s3_class(x, "sf")
  expect_identical(nrow(x), 39L)
  expect_s3_class(x$geometry, "sfc_LINESTRING")

  expect_snapshot_data(x, "fwa_join_stream_segments_elev")
})

test_that("fwa_join_stream_segments elevation elevatr", {
  skip_on_runiverse()
  skip_if_not_installed("elevatr")

  watershed <- fwa_add_watershed_to_blk(data.frame(blk = 356308001, rm = 1000))
  network <- fwa_add_collection_to_polygon(watershed)
  streams <- dplyr::select(network, blk = blue_line_key)
  x <- fwa_join_stream_segments(streams, elevation = TRUE, source = "elevatr")
  expect_s3_class(x, "sf")
  expect_identical(nrow(x), 39L)
  expect_s3_class(x$geometry, "sfc_LINESTRING")
})

test_that("fwa_join_stream_segments without source soft-deprecated if elevation", {
  local_mocked_bindings(
    fwa_add_gm_elevation_to_point = function(x, ...) {
      x$elevation <- 0
      x
    }
  )
  rlang::local_options(lifecycle_verbosity = "warning")
  streams <- sf::st_sf(
    blk = 1L,
    geometry = sf::st_sfc(
      sf::st_linestring(matrix(c(0, 0, 0, 1), ncol = 2, byrow = TRUE)),
      crs = 3005
    )
  )
  expect_no_warning(fwa_join_stream_segments(streams))
  expect_snapshot(x <- fwa_join_stream_segments(streams, elevation = TRUE))
})

test_that("fwa_join_stream_segments errors with invalid source", {
  streams <- sf::st_sf(
    blk = 1L,
    geometry = sf::st_sfc(
      sf::st_linestring(matrix(c(0, 0, 0, 1), ncol = 2, byrow = TRUE)),
      crs = 3005
    )
  )
  expect_error(fwa_join_stream_segments(streams, source = "other"))
  expect_error(fwa_join_stream_segments(streams, zoom = 0))
})

test_that("fwa_join_stream_segments diff_name", {
  watershed <- fwa_add_watershed_to_blk(data.frame(blk = 356308001, rm = 1000))
  network <- fwa_add_collection_to_polygon(watershed)

  streams <- network |>
    dplyr::as_tibble() |>
    dplyr::select(blk = blue_line_key, Shape = geometry) |>
    sf::st_as_sf()

  x <- fwa_join_stream_segments(streams)
  expect_s3_class(x, "sf")
  expect_identical(nrow(x), 39L)
  expect_s3_class(x$geometry, "sfc_LINESTRING")
})
