test_that("fwa_add_elevatr_elevation_to_point works", {
  skip_on_runiverse()
  skip_if_not_installed("elevatr")

  rm <- fwa_add_rms_to_blk(data.frame(blk = 356308001), interval = 1000)
  rm$fwa_elevation <- rm$elevation
  rm$elevation <- NULL
  x <- fwa_add_elevatr_elevation_to_point(rm)
  expect_s3_class(x, "sf")
  expect_identical(colnames(x), c(colnames(rm), "elevation"))
  expect_identical(x$rm, rm$rm)
  expect_type(x$elevation, "double")
  expect_false(anyNA(x$elevation))
  # terrain tiles agree with the freshwater atlas elevations to within 25 m
  expect_lt(max(abs(x$elevation - x$fwa_elevation)), 25)
})

test_that("fwa_add_elevatr_elevation_to_point zero rows", {
  skip_if_not_installed("elevatr")
  x <- sf::st_sf(geometry = sf::st_sfc(sf::st_point(c(0, 0)), crs = 3005))[0, ]
  x <- fwa_add_elevatr_elevation_to_point(x)
  expect_identical(x$elevation, numeric(0))
})

test_that("fwa_add_elevatr_elevation_to_point passes zoom", {
  skip_if_not_installed("elevatr")
  local_mocked_bindings(
    get_elev_point = function(locations, src, z, ...) {
      locations$elevation <- rep(z, nrow(locations))
      locations
    },
    .package = "elevatr"
  )
  x <- sf::st_sf(
    blk = 1:2,
    geometry = sf::st_sfc(
      sf::st_point(c(1000000, 500000)),
      sf::st_point(c(1000100, 500000)),
      crs = 3005
    )
  )
  expect_identical(fwa_add_elevatr_elevation_to_point(x)$elevation, c(12, 12))
  expect_identical(
    fwa_add_elevatr_elevation_to_point(x, zoom = 10)$elevation,
    c(10, 10)
  )
})

test_that("fwa_add_elevatr_elevation_to_point errors with invalid zoom", {
  skip_if_not_installed("elevatr")
  x <- sf::st_sf(
    geometry = sf::st_sfc(sf::st_point(c(1000000, 500000)), crs = 3005)
  )
  expect_error(fwa_add_elevatr_elevation_to_point(x, zoom = 15))
})
