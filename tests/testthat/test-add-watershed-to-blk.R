test_that("fwa_add_watershed_to_blk works", {
  rlang::local_options(nocache = TRUE)

  x <- fwa_add_watershed_to_blk(data.frame(blk = 360879896L))

  expect_s3_class(x, "sf")
  expect_identical(colnames(x), c("blk", "rm", "geometry"))
  expect_identical(x$blk, 360879896L)
  expect_identical(x$rm, 0)
  expect_s3_class(x$geometry, "sfc_POLYGON")
  expect_equal(as.numeric(sf::st_area(x)), 154167602.934787)
})

test_that("fwa_add_watershed_to_blk works 360883036 rm = 10", {
  rlang::local_options(nocache = TRUE)

  x <- fwa_add_watershed_to_blk(data.frame(blk = 360883036, rm = 10))
  expect_s3_class(x, "sf")
  expect_identical(colnames(x), c("blk", "rm", "geometry"))
  expect_identical(x$rm, 10)
  expect_identical(x$blk, 360883036)
  expect_s3_class(x$geometry, "sfc_POLYGON")
  expect_equal(as.numeric(sf::st_area(x)), 36479050.6256187)
})

test_that("fwa_add_watershed_to_blk exclude excludes", {
  rlang::local_options(nocache = TRUE)

  x <- fwa_add_watershed_to_blk(
    data.frame(blk = 360883036, rm = 10),
    exclude = TRUE
  )
  expect_s3_class(x, "sf")
  expect_identical(colnames(x), c("blk", "rm", "geometry"))
  expect_identical(x$rm, 10)
  expect_identical(x$blk, 360883036)
  expect_s3_class(x$geometry, "sfc_POLYGON")
  expect_equal(as.numeric(sf::st_area(x)), 33609950.4141863)
})

test_that("fwa_add_watershed_to_blk doesn't work when rm = 0 for 360883036 even with exclude", {
  rlang::local_options(nocache = TRUE)

  chk::expect_chk_error(fwa_add_watershed_to_blk(
    data.frame(blk = 360883036),
    exclude = TRUE
  ))
})

test_that("fwa_add_watershed_to_blk empty polygon when no watershed returned", {
  local_mocked_bindings(
    fwa_watershed_at_measure = function(blue_line_key, ...) {
      if (blue_line_key %in% c(1, 4)) {
        return(sf::st_sf(geometry = sf::st_sfc(crs = 3005)))
      }
      sf::st_sf(
        refine_method = "KEEP",
        geometry = sf::st_sfc(
          sf::st_polygon(list(rbind(c(0, 0), c(1, 0), c(1, 1), c(0, 0)))),
          crs = 3005
        )
      )
    }
  )
  expect_snapshot(
    x <- fwa_add_watershed_to_blk(data.frame(blk = c(1, 2, 3), rm = 0))
  )
  expect_s3_class(x, "sf")
  expect_identical(colnames(x), c("blk", "rm", "geometry"))
  expect_identical(x$blk, c(1, 2, 3))
  expect_s3_class(x$geometry, "sfc_POLYGON")
  expect_identical(sf::st_is_empty(x), c(TRUE, FALSE, FALSE))
  expect_snapshot(
    x <- fwa_add_watershed_to_blk(data.frame(blk = c(1, 2, 4), rm = 0))
  )
})

test_that("fwa_add_watershed_to_blk empty polygon when rm beyond end of stream", {
  rlang::local_options(nocache = TRUE)
  expect_warning(
    x <- fwa_add_watershed_to_blk(data.frame(blk = 356308001, rm = 20000)),
    "1 watershed is empty as no watershed was returned"
  )
  expect_true(sf::st_is_empty(x))
})
