test_that("watershed functions work", {
  ### xy to watershed
  x <- -131.927
  y <- 54.032
  epsg <- 4326

  wshed <- fwa_xy_to_watershed(x = x, y = y, epsg = epsg)
  expect_is(wshed, "sf")
  expect_identical(nrow(wshed), 1L)
  expect_identical(names(wshed), c("area_ha", "geometry"))

  ### point to watershed
  sfc <- sf::st_as_sfc(glue::glue("POINT({x} {y})"), crs = 4326)
  sf <- sf::st_as_sf(sfc)

  epsg <- sf::st_crs(sf, parameters = TRUE)

  wshed <- fwa_point_to_watershed(sfc)
  expect_is(wshed, "sf")
  expect_identical(nrow(wshed), 1L)
  expect_identical(names(wshed), c("area_ha", "geometry"))

  wshed <- fwa_point_to_watershed(sf)
  expect_is(wshed, "sf")
  expect_identical(nrow(wshed), 1L)
  expect_identical(names(wshed), c("area_ha", "geometry"))

  ### blk to watershed
  blk <- 356308001
  drm <- 10
  wshed <- fwa_blue_line_key_to_watershed(blue_line_key = blk,
                                          distance_from_mouth = drm)
  expect_is(wshed, "sf")
  expect_identical(nrow(wshed), 1L)
  expect_identical(names(wshed), c("blue_line_key", "distance_from_mouth", "area_ha", "geometry"))

  # test when error if 0
  blk <- 356528119
  drm <- 0
  expect_error(fwa_blue_line_key_to_watershed(blue_line_key = blk,
                                          distance_from_mouth = drm))

})
