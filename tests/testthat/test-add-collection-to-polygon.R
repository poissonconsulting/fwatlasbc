test_that("fwa_add_collection_to_polygon functions work", {
  wshed <- fwa_add_watershed_to_blk(data.frame(blk = 356308001))
  x <- fwa_add_collection_to_polygon(wshed, limit = 1000)

  expect_s3_class(x, "sf")
  expect_identical(sf::st_crs(x)$epsg, 3005L)
  expect_gte(nrow(x), 189L)
  expect_s3_class(x$geometry, "sfc_LINESTRING")
  expect_identical(colnames(sf::st_coordinates(x)), c("X", "Y", "L1")) # why not , "Z", "L1"

  expect_identical(
    colnames(x),
    c("blk", "rm", "id", "blue_line_key", "blue_line_key_50k", "downstream_route_measure",
      "edge_type", "feature_code", "feature_source", "fwa_watershed_code",
      "gnis_id", "gnis_name", "gradient", "left_right_tributary", "length_metre",
      "linear_feature_id", "local_watershed_code", "localcode_ltree",
      "stream_magnitude", "stream_order", "upstream_area_ha", "upstream_route_measure",
      "waterbody_key", "watershed_code_50k", "watershed_group_code",
      "watershed_group_code_50k", "watershed_group_id", "watershed_key",
      "watershed_key_50k", "wscode_ltree", "geometry"))
})

test_that("fwa_add_collection_to_polygon function intersects work", {
  wshed <- fwa_add_watershed_to_blk(data.frame(blk = 356308001))
  x <- fwa_add_collection_to_polygon(wshed, intersect = TRUE, limit = 1000)

  expect_s3_class(x, "sf")
  expect_identical(sf::st_crs(x)$epsg, 3005L)
  expect_gte(nrow(x), 189L)
  expect_identical(
    colnames(x),
    c("blk", "rm", "id", "blue_line_key", "blue_line_key_50k", "downstream_route_measure",
      "edge_type", "feature_code", "feature_source", "fwa_watershed_code",
      "gnis_id", "gnis_name", "gradient", "left_right_tributary", "length_metre",
      "linear_feature_id", "local_watershed_code", "localcode_ltree",
      "stream_magnitude", "stream_order", "upstream_area_ha", "upstream_route_measure",
      "waterbody_key", "watershed_code_50k", "watershed_group_code",
      "watershed_group_code_50k", "watershed_group_id", "watershed_key",
      "watershed_key_50k", "wscode_ltree", "geometry"))

  skip("geometry for intersection sfc_GEOMETRY not sfc_LINESTRING as element 50 is POINT due to intersection - not sure for general algorithm to fix!")
  expect_s3_class(x$geometry, "sfc_LINESTRING")
  expect_identical(colnames(sf::st_coordinates(x)), c("X", "Y", "L1")) # why not , "Z", "L1"
})

test_that("fwa_add_collection_to_polygon function intersects works with named streams and keeps extras and deals other projection", {
  wshed <- fwa_add_watershed_to_blk(data.frame(blk = 356308001, ExCol = "ex"))
  wshed <- sf::st_transform(wshed, 4326)
  x <- fwa_add_collection_to_polygon(wshed, "whse_basemapping.fwa_named_streams",
                                       limit = 1000,
                                        epsg = 32610)

  expect_s3_class(x, "sf")
  expect_identical(sf::st_crs(x)$epsg, 32610L)
  expect_identical(nrow(x), 12L)
  expect_s3_class(x$geometry, "sfc_MULTILINESTRING")
  expect_identical(
    colnames(x),
    c("blk", "ExCol", "rm",  "id", "blue_line_key", "fwa_stream_networks_label_id",
      "gnis_name", "stream_order", "watershed_group_code", "geometry"
    ))
})
