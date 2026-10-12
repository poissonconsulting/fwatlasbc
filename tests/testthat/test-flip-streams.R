make_flip_streams <- function() {
  ls <- function(...) sf::st_linestring(matrix(c(...), ncol = 2, byrow = TRUE))
  sf::st_sf(
    blk = c(1L, 2L, 3L),
    id = 1:3,
    geometry = sf::st_sfc(
      ls(0, 0, 0, 500, 0, 1000), # flowing upstream
      ls(100, 1000, 100, 0), # flowing downstream
      ls(200, 0, 200, 1000), # flat
      crs = 3005
    )
  )
}

mock_elevation <- function(x, ...) {
  y <- sf::st_coordinates(x)[, "Y"]
  x$elevation <- dplyr::if_else(
    sf::st_coordinates(x)[, "X"] == 200,
    10,
    y / 10
  )
  x
}

test_that("fwa_flip_streams flips streams flowing upstream", {
  local_mocked_bindings(fwa_add_gm_elevation_to_point = mock_elevation)
  streams <- make_flip_streams()
  x <- fwa_flip_streams(streams)
  expect_s3_class(x, "sf")
  expect_identical(colnames(x), colnames(streams))
  expect_identical(x$id, streams$id)
  expect_identical(sf::st_crs(x), sf::st_crs(streams))
  expect_identical(
    sf::st_coordinates(x$geometry[[1]]),
    sf::st_coordinates(streams$geometry[[1]])[3:1, ]
  )
  expect_identical(x$geometry[[2]], streams$geometry[[2]])
  expect_identical(x$geometry[[3]], streams$geometry[[3]])
})

test_that("fwa_flip_streams no flips", {
  local_mocked_bindings(fwa_add_gm_elevation_to_point = mock_elevation)
  streams <- make_flip_streams()[2:3, ]
  x <- fwa_flip_streams(streams)
  expect_identical(x, streams)
})

test_that("fwa_flip_streams zero rows", {
  streams <- make_flip_streams()[0, ]
  expect_identical(fwa_flip_streams(streams), streams)
})

test_that("fwa_flip_streams errors with non-linestrings", {
  streams <- make_flip_streams() |>
    sf::st_cast("MULTILINESTRING")
  expect_error(fwa_flip_streams(streams))
})

test_that("fwa_flip_streams errors without blk", {
  streams <- make_flip_streams()
  streams$blk <- NULL
  expect_error(fwa_flip_streams(streams))
})

test_that("fwa_flip_streams works with google elevation", {
  skip_on_runiverse()
  skip_if(Sys.getenv("GOOGLE_MAPS_ELEVATION_API_KEY") == "")
  streams <- fwa_add_rms_to_blk(data.frame(blk = 356308001)) |>
    fwa_convert_rms_to_streams() |>
    dplyr::select("blk")
  upstream <- streams
  upstream$geometry[[1]] <- reverse_linestring(streams$geometry[[1]])
  x <- fwa_flip_streams(upstream)
  y <- fwa_flip_streams(streams)
  expect_identical(sf::st_geometry(x), sf::st_geometry(y))
})
