test_that("fwa_convert_streams_to_rms", {
  watershed <- fwa_add_watershed_to_blk(data.frame(blk = 356308001, rm = 1000))
  network <- fwa_add_collection_to_polygon(watershed)
  streams <- dplyr::select(network, blk = blue_line_key) |>
    dplyr::filter(blk %in% c("355992255", "356308001"))
  x <- fwa_convert_streams_to_rms(streams, interval = 100)
  expect_s3_class(x, "sf")
  expect_identical(nrow(x), 235L)
  expect_s3_class(x$geometry, "sfc_POINT")

  expect_snapshot_data(x, "fwa_convert_streams_to_rms")
})

test_that("fwa_convert_streams_to_rms elevation", {
  watershed <- fwa_add_watershed_to_blk(data.frame(blk = 356308001, rm = 1000))
  network <- fwa_add_collection_to_polygon(watershed)
  streams <- dplyr::select(network, blk = blue_line_key)
  x <- fwa_convert_streams_to_rms(streams, interval = 100, elevation = TRUE)
  expect_s3_class(x, "sf")
  expect_identical(nrow(x), 903L)
  expect_s3_class(x$geometry, "sfc_POINT")

  expect_snapshot_data(x, "fwa_convert_streams_to_rms_elev")
})

test_that("fwa_convert_streams_to_rms diff_name and not end", {
  watershed <- fwa_add_watershed_to_blk(data.frame(blk = 356308001, rm = 1000))
  network <- fwa_add_collection_to_polygon(watershed)

  streams <- network |>
    dplyr::as_tibble() |>
    dplyr::select(blk = blue_line_key, Shape = geometry) |>
    sf::st_as_sf()

  x <- fwa_convert_streams_to_rms(streams, interval = 100, end = 100 + 1)
  expect_s3_class(x, "sf")
  expect_identical(nrow(x), 864L)
  expect_s3_class(x$geometry, "sfc_POINT")
})
