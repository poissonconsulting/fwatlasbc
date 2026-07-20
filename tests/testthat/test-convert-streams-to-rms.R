test_that("fwa_convert_streams_to_rms works with a single stream (no parent)", {
  linestring <- sf::st_linestring(
    matrix(c(0, 0, 0, 100, 0, 200), ncol = 2, byrow = TRUE)
  )
  streams <- sf::st_sf(
    blk = 1L,
    geometry = sf::st_sfc(linestring, crs = 3005)
  )
  x <- fwa_convert_streams_to_rms(streams, interval = 50)
  expect_s3_class(x, "sf")
  expect_identical(nrow(x), 5L)
  expect_s3_class(x$geometry, "sfc_POINT")
  expect_identical(x$rm, c(0L, 50L, 100L, 150L, 200L))
  expect_true(all(is.na(x$parent_blk)))
  expect_true(all(is.na(x$parent_rm)))
})

test_that("fwa_convert_streams_to_rms", {
  watershed <- fwa_add_watershed_to_blk(data.frame(blk = 356308001, rm = 1000))
  network <- fwa_add_collection_to_polygon(watershed)
  streams <- dplyr::select(network, blk = blue_line_key) |>
    dplyr::filter(blk %in% c("355992255", "356308001"))
  x <- fwa_convert_streams_to_rms(streams, interval = 100, end = 1)
  expect_s3_class(x, "sf")
  expect_identical(nrow(x), 235L)
  expect_s3_class(x$geometry, "sfc_POINT")

  expect_snapshot_data(x, "fwa_convert_streams_to_rms", digits = 9)
})

test_that("fwa_convert_streams_to_rms reverse", {
  skip_on_runiverse()

  watershed <- fwa_add_watershed_to_blk(data.frame(blk = 356308001, rm = 1000))
  network <- fwa_add_collection_to_polygon(watershed)
  streams <- dplyr::select(network, blk = blue_line_key) |>
    dplyr::filter(blk %in% c("355992255", "356308001"))
  x <- fwa_convert_streams_to_rms(
    streams,
    interval = 100,
    end = 1,
    reverse = 356308001L
  )
  expect_s3_class(x, "sf")
  expect_identical(nrow(x), 235L)
  expect_s3_class(x$geometry, "sfc_POINT")

  expect_snapshot_data(x, "fwa_convert_streams_to_rms_rev", digits = 9)
})

test_that("fwa_convert_streams_to_rms inaccuracy", {
  watershed <- fwa_add_watershed_to_blk(data.frame(blk = 356308001, rm = 1000))
  network <- fwa_add_collection_to_polygon(watershed)
  streams <- dplyr::select(network, blk = blue_line_key) |>
    dplyr::filter(blk %in% c("355992255", "356308001"))
  streams$geometry[streams$blk == 356308001] <- streams$geometry[
    streams$blk == 356308001
  ] +
    10
  x <- fwa_convert_streams_to_rms(streams, interval = 100, end = 1, gap = 10)
  expect_s3_class(x, "sf")
  expect_identical(nrow(x), 235L)
  expect_s3_class(x$geometry, "sfc_POINT")

  expect_snapshot_data(x, "fwa_convert_streams_to_rms_inaccuracy", digits = 9)
})

test_that("fwa_convert_streams_to_rms tiny inaccuracy", {
  watershed <- fwa_add_watershed_to_blk(data.frame(blk = 356308001, rm = 1000))
  network <- fwa_add_collection_to_polygon(watershed)
  streams <- dplyr::select(network, blk = blue_line_key) |>
    dplyr::filter(blk %in% c("355992255", "356308001"))
  streams$geometry[streams$blk == 355992255] <- streams$geometry[
    streams$blk == 355992255
  ] +
    0.000001
  x <- fwa_convert_streams_to_rms(streams, interval = 100, end = 1, gap = 10)
  expect_s3_class(x, "sf")
  expect_identical(nrow(x), 235L)
  expect_s3_class(x$geometry, "sfc_POINT")

  expect_snapshot_data(x, "fwa_convert_streams_to_rms_tinac", digits = 9)
})

test_that("fwa_convert_streams_to_rms just make end", {
  watershed <- fwa_add_watershed_to_blk(data.frame(blk = 356308001, rm = 1000))
  network <- fwa_add_collection_to_polygon(watershed)
  streams <- dplyr::select(network, blk = blue_line_key) |>
    dplyr::filter(blk %in% c("355992255", "356308001"))
  x <- fwa_convert_streams_to_rms(streams, interval = 100, end = 34)
  expect_s3_class(x, "sf")
  expect_identical(nrow(x), 235L)
  expect_s3_class(x$geometry, "sfc_POINT")

  expect_snapshot_data(x, "fwa_convert_streams_to_rms_34", digits = 9)
})

test_that("fwa_convert_streams_to_rms not make end", {
  watershed <- fwa_add_watershed_to_blk(data.frame(blk = 356308001, rm = 1000))
  network <- fwa_add_collection_to_polygon(watershed)
  streams <- dplyr::select(network, blk = blue_line_key) |>
    dplyr::filter(blk %in% c("355992255", "356308001"))
  x <- fwa_convert_streams_to_rms(streams, interval = 100, end = 35)
  expect_s3_class(x, "sf")
  expect_identical(nrow(x), 234L)
  expect_s3_class(x$geometry, "sfc_POINT")

  expect_snapshot_data(x, "fwa_convert_streams_to_rms_35", digits = 9)
})

test_that("fwa_convert_streams_to_rms elevation", {
  skip_on_runiverse()

  watershed <- fwa_add_watershed_to_blk(data.frame(blk = 356308001, rm = 1000))
  network <- fwa_add_collection_to_polygon(watershed)
  streams <- dplyr::select(network, blk = blue_line_key)
  x <- fwa_convert_streams_to_rms(
    streams,
    interval = 100,
    elevation = TRUE,
    end = 1
  )
  expect_s3_class(x, "sf")
  expect_identical(nrow(x), 903L)
  expect_s3_class(x$geometry, "sfc_POINT")

  skip_on_os("linux")
  expect_snapshot_data(x, "fwa_convert_streams_to_rms_elev", digits = 9)
})

test_that("fwa_convert_streams_to_rms diff_name and end = NULL", {
  watershed <- fwa_add_watershed_to_blk(data.frame(blk = 356308001, rm = 1000))
  network <- fwa_add_collection_to_polygon(watershed)

  streams <- network |>
    dplyr::as_tibble() |>
    dplyr::select(blk = blue_line_key, Shape = geometry) |>
    sf::st_as_sf()

  x <- fwa_convert_streams_to_rms(streams, interval = 100)
  expect_s3_class(x, "sf")
  expect_identical(nrow(x), 864L)
  expect_s3_class(x$geometry, "sfc_POINT")
})
