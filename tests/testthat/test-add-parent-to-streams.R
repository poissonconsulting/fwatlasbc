make_streams <- function() {
  ls <- function(...) sf::st_linestring(matrix(c(...), ncol = 2, byrow = TRUE))
  sf::st_sf(
    blk = c(1L, 1L, 2L, 3L, 4L),
    id = 1:5,
    geometry = sf::st_sfc(
      ls(0, 0, 0, 500), # mainstem segment 1 (mouth at 0, 0)
      ls(0, 500, 0, 1000), # mainstem segment 2
      ls(0, 400, 300, 400), # tributary joining at rm 400
      ls(5, 700, 300, 700), # tributary with mouth 5 m from mainstem
      ls(5000, 0, 5000, 100), # isolated stream
      crs = 3005
    )
  )
}

test_that("fwa_add_parent_to_streams adds parents", {
  streams <- make_streams()
  x <- fwa_add_parent_to_streams(streams)
  expect_s3_class(x, "sf")
  expect_identical(nrow(x), nrow(streams))
  expect_identical(x$id, streams$id)
  expect_identical(sf::st_geometry(x), sf::st_geometry(streams))
  expect_identical(x$parent_blk, c(NA, NA, 1L, NA, NA))
  expect_equal(x$parent_rm, c(NA, NA, 400, NA, NA))
  expect_equal(x$distance_to_parent, c(NA, NA, 0, NA, NA))
})

test_that("fwa_add_parent_to_streams respects gap", {
  x <- fwa_add_parent_to_streams(make_streams(), gap = 10)
  expect_identical(x$parent_blk, c(NA, NA, 1L, 1L, NA))
  # parent_rm is approximate (within gap) when the mouth is not on the parent
  expect_equal(x$parent_rm, c(NA, NA, 400, 700, NA), tolerance = 10 / 700)
  expect_equal(x$distance_to_parent, c(NA, NA, 0, 5, NA))
})

test_that("fwa_add_parent_to_streams parent_rm measured from parent mouth", {
  streams <- make_streams()
  streams <- streams[streams$blk != 1L, ] |>
    rbind(
      sf::st_sf(
        blk = 1L,
        id = 6L,
        geometry = sf::st_sfc(
          sf::st_linestring(matrix(c(0, 1000, 0, 0), ncol = 2, byrow = TRUE)),
          crs = 3005
        )
      )
    )
  x <- fwa_add_parent_to_streams(streams)
  expect_equal(x$parent_rm[x$blk == 2L], 600)
})

test_that("fwa_add_parent_to_streams single stream", {
  x <- fwa_add_parent_to_streams(make_streams()[1, ])
  expect_identical(x$parent_blk, NA_integer_)
  expect_identical(x$parent_rm, NA_real_)
  expect_identical(x$distance_to_parent, NA_real_)
})

test_that("fwa_add_parent_to_streams errors with existing parent columns", {
  streams <- make_streams()
  streams$parent_blk <- 1L
  expect_error(fwa_add_parent_to_streams(streams))
})

test_that("fwa_add_parent_to_streams errors with existing distance_to_parent", {
  streams <- make_streams()
  streams$distance_to_parent <- 1
  expect_error(fwa_add_parent_to_streams(streams))
})
