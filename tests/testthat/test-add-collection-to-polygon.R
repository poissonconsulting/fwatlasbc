test_that("fwa_add_collection_to_polygon functions work", {
  rlang::local_options(nocache = TRUE)

  wshed <- fwa_add_watershed_to_blk(data.frame(blk = 356308001))
  x <- fwa_add_collection_to_polygon(wshed, limit = 1000)

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
      "stream_magnitude", "stream_order",
      "upstream_route_measure",
      "waterbody_key", "watershed_code_50k", "watershed_group_code",
      "watershed_group_code_50k", "watershed_group_id", "watershed_key",
      "watershed_key_50k", "wscode_ltree", "geometry"))
})

test_that("fwa_add_collection_to_polygon function intersects work", {
  rlang::local_options(nocache = TRUE)

  wshed <- fwa_add_watershed_to_blk(data.frame(blk = 356308001))
  x <- fwa_add_collection_to_polygon(wshed, intersect = TRUE, limit = 1000)

  expect_s3_class(x, "sf")
  expect_identical(sf::st_crs(x)$epsg, 3005L)
  expect_gte(nrow(x), 189L)
  expect_identical(
    colnames(x),
    c("blk", "rm", "blue_line_key", "blue_line_key_50k", "downstream_route_measure",
      "edge_type", "feature_code", "feature_source", "fwa_watershed_code",
      "gnis_id", "gnis_name", "gradient", "left_right_tributary", "length_metre",
      "linear_feature_id", "local_watershed_code", "localcode_ltree",
      "stream_magnitude", "stream_order",
      "upstream_route_measure",
      "waterbody_key", "watershed_code_50k", "watershed_group_code",
      "watershed_group_code_50k", "watershed_group_id", "watershed_key",
      "watershed_key_50k", "wscode_ltree", "geometry"))

  expect_s3_class(x$geometry, "sfc_GEOMETRY")
})

test_that("fwa_add_collection_to_polygon function intersects works with named streams and keeps extras and deals other projection", {
  rlang::local_options(nocache = TRUE)

  wshed <- fwa_add_watershed_to_blk(data.frame(blk = 356308001, ExCol = "ex"))
  wshed <- sf::st_transform(wshed, 4326)
  x <- fwa_add_collection_to_polygon(wshed, "whse_basemapping.fwa_named_streams",
                                     limit = 1000,
                                     epsg = 32610)

  expect_s3_class(x, "sf")
  expect_identical(sf::st_crs(x)$epsg, 32610L)
  expect_identical(
    colnames(x),
    c("blk", "ExCol", "rm", "blue_line_key",
      "gnis_name", "named_streams_id", "stream_order", "watershed_group_code", "geometry"
    ))

  # varies between operating systems
  expect_gte(nrow(x), 10L)
  expect_lte(nrow(x), 12L)

  expect_s3_class(x$geometry[[1]], "LINESTRING")

  skip_on_os("windows") # getting 'sfc_MULTILINESTRING'/'sfc' on windows!
  expect_s3_class(x$geometry, "sfc_GEOMETRY")
})
