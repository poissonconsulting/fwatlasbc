test_that("get functions work", {
  blk <- 356308001
  wshed <- fwa_blue_line_key_to_watershed(blue_line_key = blk)

  x <- fwa_get_stream_network(wshed, limit = 1000)
  expect_is(x, "sf")
  expect_is(x$geometry, "sfc_LINESTRING")
  expect_identical(nrow(x), 89L)

  x <- fwa_get_lakes(wshed)
  expect_is(x, "sf")
  expect_is(x$geometry, "sfc_MULTIPOLYGON")
  expect_identical(nrow(x), 5L)

  x <- fwa_get_cultural_lines(wshed)
  expect_is(x, "sf")
  expect_is(x$geometry, "sfc_LINESTRING")
  expect_identical(nrow(x), 15L)

  x <- fwa_get_cultural_points(wshed)
  expect_is(x, "sf")
  expect_is(x$geometry, "sfc_POINT")
  expect_identical(nrow(x), 123L)

  x <- fwa_get_manmade_waterbodies(wshed)
  expect_is(x, "sf")
  expect_is(x$geometry, "sfc_MULTIPOLYGON")
  expect_identical(nrow(x), 2L)

  x <- fwa_get_named_streams(wshed)
  expect_is(x, "sf")
  expect_is(x$geometry, "sfc_MULTILINESTRING")
  expect_identical(nrow(x), 11L)

  # x <- fwa_get_glaciers(wshed)
  # expect_is(x, "sf")
  # expect_is(x$geometry, "sfc_MULTILINESTRING")
  # expect_identical(nrow(x), 11L)

  # x <- fwa_get_obstructions(wshed)
  # expect_is(x, "sf")
  # expect_is(x$geometry, "sfc_MULTILINESTRING")
  # expect_identical(nrow(x), 11L)

  x <- fwa_get_railway_tracks(wshed)
  expect_is(x, "sf")
  expect_is(x$geometry, "sfc_LINESTRING")
  expect_identical(nrow(x), 40L)

  x <- fwa_get_rivers(wshed)
  expect_is(x, "sf")
  expect_is(x$geometry, "sfc_MULTIPOLYGON")
  expect_identical(nrow(x), 6L)

  x <- fwa_get_transmission_lines(wshed)
  expect_is(x, "sf")
  expect_is(x$geometry, "sfc_LINESTRING")
  expect_identical(nrow(x), 6L)

  # x <- fwa_get_transport_lines(wshed)
  # expect_is(x, "sf")
  # expect_is(x$geometry, "sfc_MULTILINESTRING")
  # expect_identical(nrow(x), 721L)

  # x <- fwa_get_wetlands(wshed)
  # expect_is(x, "sf")
  # class(x$geometry)
  # expect_is(x$geometry, "sfc_LINESTRING")
  # expect_identical(nrow(x), 6L)

})
