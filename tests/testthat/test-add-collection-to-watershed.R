test_that("fwa_add_collection_to_watershed functions work", {
  rlang::local_options(lifecycle_verbosity = "quiet")

  wshed <- fwa_add_watershed_to_blk(data.frame(blk = 356308001))
  lifecycle::expect_deprecated(
    x <- fwa_add_collection_to_watershed(wshed, limit = 1000)
  )

  expect_s3_class(x, "sf")
  expect_identical(sf::st_crs(x)$epsg, 3005L)
  expect_gte(nrow(x), 189L)
  expect_s3_class(x$geometry, "sfc_LINESTRING")
  expect_identical(colnames(sf::st_coordinates(x)), c("X", "Y", "L1")) # why not , "Z", "L1"

  expect_identical(
    colnames(x),
    c("blk", "rm", "blue_line_key", "blue_line_key_50k", "downstream_route_measure",
      "edge_type", "feature_code", "feature_source", "fwa_watershed_code",
      "gnis_id", "gnis_name", "gradient", "left_right_tributary", "length_metre",
      "linear_feature_id", "local_watershed_code", "localcode_ltree",
      "stream_magnitude", "stream_order", "stream_order_max", "stream_order_parent", "upstream_route_measure",
      "waterbody_key", "watershed_code_50k", "watershed_group_code",
      "watershed_group_code_50k", "watershed_group_id", "watershed_key",
      "watershed_key_50k", "wscode_ltree", "geometry"))
})
