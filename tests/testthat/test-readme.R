test_that("multiplication works", {
  streams <- fwa_find_stream_names("steep c")
  expect_identical(streams, dplyr::tibble(stream_name = c("Steep Canyon Creek", "Steep Creek")))
  blks <- fwa_add_blks_to_stream_name(streams)
  expect_identical(blks, dplyr::tibble(stream_name = c("Steep Canyon Creek", "Steep Creek", "Steep Creek", "Steep Creek"), blk = c(360883036L, 356362258L, 356534225L, 356570155L)))
  blks <- blks[blks$blk == 356534225,]
  wshed <- fwa_add_watershed_to_blk(blks)

  expect_s3_class(wshed, "sf")
  expect_identical(colnames(wshed), c("stream_name", "blk", "rm", "geometry"))
  expect_identical(sf::st_crs(wshed)$epsg, 3005L)
  expect_gte(nrow(wshed), 1L)
  expect_s3_class(wshed$geometry, "sfc_POLYGON")
  expect_identical(wshed$rm, 0)
  expect_identical(colnames(sf::st_coordinates(wshed)), c("X", "Y", "L1", "L2")) # not sure why L1 and/or L2
  network <- fwa_add_collection_to_polygon(wshed, "stream_network")

  expect_identical(colnames(network), c(
    "stream_name", "blk", "rm", "blue_line_key", "blue_line_key_50k",
    "downstream_route_measure", "edge_type", "feature_code", "feature_source",
    "fwa_watershed_code", "gnis_id", "gnis_name", "gradient", "left_right_tributary",
    "length_metre", "linear_feature_id", "local_watershed_code",
    "localcode_ltree", "stream_magnitude", "stream_order", "stream_order_max",
    "stream_order_parent", "upstream_route_measure", "waterbody_key",
    "watershed_code_50k", "watershed_group_code", "watershed_group_code_50k",
    "watershed_group_id", "watershed_key", "watershed_key_50k", "wscode_ltree",
    "geometry"))
  skip("Columns 'blk' and 'rm' in `rms` constructed from `x` must be a unique key.")
  rms <- fwa_convert_stream_network_to_rms(network, interval = 100)
})
