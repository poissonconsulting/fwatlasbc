test_that("get functions work", {
  blk <- 356308001
  wshed <- fwa_blue_line_key_to_watershed(blue_line_key = blk)
  expect_identical(sf::st_crs(wshed)$epsg, 3005L)

  x <- fwa_get_stream_network(limit = 100)
  expect_is(x, "sf")
  expect_identical(sf::st_crs(x)$epsg, 3005L)
  expect_identical(nrow(x), 100L)
  expect_is(x$geometry, "sfc_LINESTRING")
  expect_identical(colnames(sf::st_coordinates(x)), c("X", "Y", "Z", "L1"))
#
#   x <- x$geometry[1]
#
#   sf::st_coordinates(x)
#   sf::st_is_valid(x)
#   sf::st_coordinates(sf::st_make_valid(x))

  x <- fwa_get_stream_network(wshed, limit = 1000)
  expect_is(x, "sf")
  expect_identical(sf::st_crs(x)$epsg, 3005L)
  expect_gte(nrow(x), 88L)
  expect_lte(nrow(x), 89L)
  expect_is(x$geometry, "sfc_LINESTRING")
  expect_identical(colnames(sf::st_coordinates(x)), c("X", "Y", "L1")) # why not , "Z", "L1"

  expect_identical(
    colnames(x),
    c("id", "blue_line_key", "blue_line_key_50k", "downstream_route_measure",
      "edge_type", "feature_code", "feature_source", "fwa_watershed_code",
      "gnis_id", "gnis_name", "gradient", "left_right_tributary", "length_metre",
      "linear_feature_id", "local_watershed_code", "localcode_ltree",
      "stream_magnitude", "stream_order", "upstream_area_ha", "upstream_route_measure",
      "waterbody_key", "watershed_code_50k", "watershed_group_code",
      "watershed_group_code_50k", "watershed_group_id", "watershed_key",
      "watershed_key_50k", "wscode_ltree", "geometry"))

  x <- fwa_get_lakes(wshed)
  expect_is(x, "sf")
  expect_is(x$geometry, "sfc_MULTIPOLYGON")
  expect_identical(nrow(x), 5L)
  expect_identical(sf::st_crs(x)$epsg, 3005L)
  expect_identical(
    colnames(x),
    c("id", "area_ha", "blue_line_key", "feature_code", "fwa_watershed_code",
      "gnis_id_1", "gnis_id_2", "gnis_id_3", "gnis_name_1", "gnis_name_2",
      "gnis_name_3", "left_right_tributary", "local_watershed_code",
      "localcode_ltree", "waterbody_key", "waterbody_key_50k", "waterbody_key_group_code_50k",
      "waterbody_poly_id", "waterbody_type", "watershed_code_50k",
      "watershed_group_code", "watershed_group_code_50k", "watershed_group_id",
      "watershed_key", "wscode_ltree", "geometry"))

  x <- fwa_get_cultural_lines(wshed)
  expect_is(x, "sf")
  expect_identical(sf::st_crs(x)$epsg, 3005L)
  expect_identical(nrow(x), 15L)
  expect_is(x$geometry, "sfc_LINESTRING")
  expect_identical(
    colnames(x),
    c("admit_day", "admit_mnth", "admit_rfc", "admit_rkey", "admit_srel",
      "admit_year", "asrc_day", "asrc_mnth", "asrc_year", "bcgs_tile",
      "elevation", "fcode", "feature_length_m", "id", "objectid", "ogc_fid",
      "retir_day", "retir_mnth", "retir_rfc", "retir_rkey", "retir_srel",
      "retir_year", "rsrc_day", "rsrc_mnth", "rsrc_year", "se_anno_cad_data",
      "source_id", "x_accuracy", "x_capture", "x_level", "x_reason",
      "x_supplier", "geometry"))

  x <- fwa_get_cultural_points(wshed)
  expect_is(x, "sf")
  expect_identical(sf::st_crs(x)$epsg, 3005L)
  expect_identical(nrow(x), 123L)
  expect_is(x$geometry, "sfc_POINT")
  expect_identical(
    colnames(x),
    c("admit_day", "admit_mnth", "admit_rfc", "admit_rkey", "admit_srel",
      "admit_year", "asrc_day", "asrc_mnth", "asrc_year", "bcgs_tile",
      "elevation", "fcode", "id", "objectid", "ogc_fid", "retir_day",
      "retir_mnth", "retir_rfc", "retir_rkey", "retir_srel", "retir_year",
      "rotation", "rsrc_day", "rsrc_mnth", "rsrc_year", "se_anno_cad_data",
      "source_id", "x_accuracy", "x_capture", "x_level", "x_reason",
      "x_supplier", "geometry"))

  x <- fwa_get_manmade_waterbodies(wshed)
  expect_is(x, "sf")
  expect_identical(sf::st_crs(x)$epsg, 3005L)
  expect_identical(nrow(x), 2L)
  expect_is(x$geometry, "sfc_MULTIPOLYGON")
  expect_identical(
    colnames(x),
    c("id", "area_ha", "blue_line_key", "feature_code", "fwa_watershed_code",
      "gnis_id_1", "gnis_id_2", "gnis_id_3", "gnis_name_1", "gnis_name_2",
      "gnis_name_3", "left_right_tributary", "local_watershed_code",
      "localcode_ltree", "waterbody_key", "waterbody_key_50k", "waterbody_key_group_code_50k",
      "waterbody_poly_id", "waterbody_type", "watershed_code_50k",
      "watershed_group_code", "watershed_group_code_50k", "watershed_group_id",
      "watershed_key", "wscode_ltree", "geometry"))

  x <- fwa_get_named_streams(wshed)
  expect_is(x, "sf")
  expect_identical(sf::st_crs(x)$epsg, 3005L)
  expect_identical(nrow(x), 11L)
  expect_is(x$geometry, "sfc_MULTILINESTRING")
  expect_identical(
    colnames(x),
    c("id", "blue_line_key", "fwa_stream_networks_label_id", "gnis_name",
      "stream_order", "watershed_group_code", "geometry"))

  # x <- fwa_get_glaciers(wshed)
  # expect_is(x, "sf")
  # expect_is(x$geometry, "sfc_MULTILINESTRING")
  # expect_identical(nrow(x), 11L)
  # expect_identical(sf::st_crs(x)$epsg, 3005L)

  # x <- fwa_get_obstructions(wshed)
  # expect_is(x, "sf")
  # expect_is(x$geometry, "sfc_MULTILINESTRING")
  # expect_identical(nrow(x), 11L)
  # expect_identical(sf::st_crs(x)$epsg, 3005L)

  x <- fwa_get_railway_tracks(wshed)
  expect_is(x, "sf")
  expect_identical(sf::st_crs(x)$epsg, 3005L)
  expect_identical(nrow(x), 40L)
  expect_is(x$geometry, "sfc_MULTILINESTRING")
  expect_identical(
    colnames(x),
    c("administrative_area", "attribute_acquisition_technque", "attribute_creation_date",
       "attribute_provider", "attribute_revision_date", "design_speed_freight",
       "design_speed_passenger", "electrification", "feature_length_m",
       "gauge", "geometry_acquisition_technque", "geometry_creation_date",
       "geometry_planimetric_accuracy", "geometry_provider", "geometry_revision_date",
       "id", "nid", "number_of_tracks", "objectid", "ogc_fid", "operator_english_name",
       "operator_subdiv_portion_end", "operator_subdiv_portion_start",
       "owner_name", "railway_track_id", "regulator", "se_anno_cad_data",
       "security_classification", "source_id", "standards_version",
       "status", "subdivision1_end", "subdivision1_name", "subdivision1_nid",
       "subdivision1_start", "subdivision2_end", "subdivision2_name",
       "subdivision2_nid", "subdivision2_start", "track_classification",
       "track_name", "track_segment_id", "track_user1_english_name",
       "track_user2_english_name", "track_user3_english_name", "track_user4_english_name",
       "transport_type", "use_type", "geometry"))

  x <- fwa_get_rivers(wshed)
  expect_is(x, "sf")
  expect_identical(sf::st_crs(x)$epsg, 3005L)
  expect_identical(nrow(x), 6L)
  expect_is(x$geometry, "sfc_MULTIPOLYGON")
  expect_identical(
    colnames(x),
    c("id", "area_ha", "blue_line_key", "feature_code", "fwa_watershed_code",
      "gnis_id_1", "gnis_id_2", "gnis_id_3", "gnis_name_1", "gnis_name_2",
      "gnis_name_3", "left_right_tributary", "local_watershed_code",
      "localcode_ltree", "waterbody_key", "waterbody_key_50k", "waterbody_key_group_code_50k",
      "waterbody_poly_id", "waterbody_type", "watershed_code_50k",
      "watershed_group_code", "watershed_group_code_50k", "watershed_group_id",
      "watershed_key", "wscode_ltree", "geometry"))

  x <- fwa_get_transmission_lines(wshed)
  expect_is(x, "sf")
  expect_identical(sf::st_crs(x)$epsg, 3005L)
  expect_identical(nrow(x), 6L)
  expect_is(x$geometry, "sfc_LINESTRING")
  expect_identical(
    colnames(x),
    c("circuit_description", "circuit_name", "feature_length_m",
      "id", "objectid", "ogc_fid", "owner", "se_anno_cad_data", "source_date",
      "transmission_line_id", "voltage", "geometry"))

  # x <- fwa_get_transport_lines(wshed)
  # expect_is(x, "sf")
  # expect_is(x$geometry, "sfc_MULTILINESTRING")
  # expect_identical(nrow(x), 721L)
  # expect_identical(sf::st_crs(x)$epsg, 3005L)


  # x <- fwa_get_wetlands(wshed)
  # expect_is(x, "sf")
  # class(x$geometry)
  # expect_is(x$geometry, "sfc_LINESTRING")
  # expect_identical(nrow(x), 6L)
  # expect_identical(sf::st_crs(x)$epsg, 3005L)
})

test_that("test get functions deal with other projections and intersect", {
  blk <- 356308001
  wshed <- fwa_blue_line_key_to_watershed(blue_line_key = blk)

  colnames <- c("id", "blue_line_key", "blue_line_key_50k", "downstream_route_measure",
    "edge_type", "feature_code", "feature_source", "fwa_watershed_code",
    "gnis_id", "gnis_name", "gradient", "left_right_tributary", "length_metre",
    "linear_feature_id", "local_watershed_code", "localcode_ltree",
    "stream_magnitude", "stream_order", "upstream_area_ha", "upstream_route_measure",
    "waterbody_key", "watershed_code_50k", "watershed_group_code",
    "watershed_group_code_50k", "watershed_group_id", "watershed_key",
    "watershed_key_50k", "wscode_ltree", "geometry")

  x <- fwa_get_stream_network(wshed)

  expect_is(x, "sf")
  expect_identical(sf::st_crs(x)$epsg, 3005L)
  expect_gte(nrow(x), 88L)
  expect_lte(nrow(x), 89L)
  expect_is(x$geometry, "sfc_LINESTRING")
  expect_identical(
    colnames(x), colnames)

  wshed <- sf::st_transform(wshed, 4326)

  x <- fwa_get_stream_network(wshed)
  expect_is(x, "sf")
  expect_identical(sf::st_crs(x)$epsg, 3005L)
  expect_gte(nrow(x), 88L)
  expect_lte(nrow(x), 89L)
  expect_is(x$geometry, "sfc_LINESTRING")
  expect_identical(
    colnames(x), colnames)

  x <- fwa_get_stream_network(wshed, epsg = 4326)

  expect_is(x, "sf")
  expect_identical(sf::st_crs(x)$epsg, 4326L)
  expect_gte(nrow(x), 88L)
  expect_lte(nrow(x), 89L)
  expect_is(x$geometry, "sfc_LINESTRING")
  expect_identical(
    colnames(x), colnames)

  wshed <- sf::st_transform(wshed, "+proj=utm +zone=11 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs")

  x <- fwa_get_stream_network(wshed, epsg = 4326)
  expect_is(x, "sf")
  expect_identical(sf::st_crs(x)$epsg, 4326L)
  expect_gte(nrow(x), 88L)
  expect_lte(nrow(x), 89L)
  expect_is(x$geometry, "sfc_LINESTRING")
  expect_identical(
    colnames(x), colnames)

  x <- fwa_get_stream_network(wshed, epsg = 4326, intersect = TRUE)

  expect_is(x, "sf")
  expect_identical(sf::st_crs(x)$epsg, 4326L)
  expect_gte(nrow(x), 88L)
  expect_lte(nrow(x), 89L)
  expect_is(x$geometry, "sfc_LINESTRING")
  expect_identical(
    colnames(x), colnames)
})
