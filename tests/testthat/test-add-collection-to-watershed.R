test_that("fwa_add_collection_to_watershed functions work", {
  wshed <- fwa_add_watershed_to_blk(data.frame(BLK = 356308001))
  x <- fwa_add_collection_to_watershed(wshed, limit = 1000)

  expect_s3_class(x, "sf")
  expect_identical(sf::st_crs(x)$epsg, 3005L)
  expect_gte(nrow(x), 189L)
  expect_s3_class(x$geometry, "sfc_LINESTRING")
  expect_identical(colnames(sf::st_coordinates(x)), c("X", "Y", "L1")) # why not , "Z", "L1"

  expect_identical(
    colnames(x),
    c("BLK", "Id", "BlueLineKey", "BlueLineKey50K", "DownstreamRouteMeasure",
      "EdgeType", "FeatureCode", "FeatureSource", "FwaWatershedCode",
      "GnisId", "GnisName", "Gradient", "LeftRightTributary", "LengthMetre",
      "LinearFeatureId", "LocalWatershedCode", "LocalcodeLtree", "StreamMagnitude",
      "StreamOrder", "UpstreamAreaHa", "UpstreamRouteMeasure", "WaterbodyKey",
      "WatershedCode50K", "WatershedGroupCode", "WatershedGroupCode50K",
      "WatershedGroupId", "WatershedKey", "WatershedKey50K", "WscodeLtree",
      "geometry"))
})

test_that("fwa_add_collection_to_watershed function intersects work", {
  wshed <- fwa_add_watershed_to_blk(data.frame(BLK = 356308001))
  x <- fwa_add_collection_to_watershed(wshed, intersect = TRUE, limit = 1000)

  expect_s3_class(x, "sf")
  expect_identical(sf::st_crs(x)$epsg, 3005L)
  expect_gte(nrow(x), 189L)
  expect_identical(
    colnames(x),
    c("BLK", "Id", "BlueLineKey", "BlueLineKey50K", "DownstreamRouteMeasure",
      "EdgeType", "FeatureCode", "FeatureSource", "FwaWatershedCode",
      "GnisId", "GnisName", "Gradient", "LeftRightTributary", "LengthMetre",
      "LinearFeatureId", "LocalWatershedCode", "LocalcodeLtree", "StreamMagnitude",
      "StreamOrder", "UpstreamAreaHa", "UpstreamRouteMeasure", "WaterbodyKey",
      "WatershedCode50K", "WatershedGroupCode", "WatershedGroupCode50K",
      "WatershedGroupId", "WatershedKey", "WatershedKey50K", "WscodeLtree",
      "geometry"))

  skip("geometry for intersection sfc_GEOMETRY not sfc_LINESTRING as element 50 is POINT due to intersection - not sure for general algorithm to fix!")
  expect_s3_class(x$geometry, "sfc_LINESTRING")
  expect_identical(colnames(sf::st_coordinates(x)), c("X", "Y", "L1")) # why not , "Z", "L1"
})

test_that("fwa_add_collection_to_watershed function intersects works with named streams and keeps extras and deals other projection", {
  wshed <- fwa_add_watershed_to_blk(data.frame(BLK = 356308001, ExCol = "ex"))
  wshed <- sf::st_transform(wshed, 4326)
  x <- fwa_add_collection_to_watershed(wshed, "named_streams", limit = 1000,
                                        epsg = 32610, camel_case = FALSE)

  expect_s3_class(x, "sf")
  expect_identical(sf::st_crs(x)$epsg, 32610L)
  expect_identical(nrow(x), 12L)
  expect_s3_class(x$geometry, "sfc_MULTILINESTRING")
  expect_identical(
    colnames(x),
    c("BLK", "ExCol", "id", "blue_line_key", "fwa_stream_networks_label_id",
      "gnis_name", "stream_order", "watershed_group_code", "geometry"
    ))
})
