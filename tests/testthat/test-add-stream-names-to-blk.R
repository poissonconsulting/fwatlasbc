test_that("fwa_add_stream_names_to_blk works simple", {
  x <- fwa_add_stream_names_to_blk(data.frame(blk = 360886335L))
  expect_identical(x, dplyr::tibble(blk = 360886335L, stream_name = "Aaltanhash River"))
})

test_that("fwa_add_stream_names_to_blk preserves sf", {
  sf <- data.frame(blk = 360886335L, x = 1, y = 2) |>
    sf::st_as_sf(coords = c("x", "y"), dim = "XY")

  x <- fwa_add_stream_names_to_blk(sf)
  expect_s3_class(x, "sf")
  expect_identical(colnames(x), c("blk", "stream_name", "geometry"))
  expect_snapshot_data(x, "add_stream_names_sf")
})

test_that("fwa_add_stream_names_to_blk works no rows", {
  x <- fwa_add_stream_names_to_blk(data.frame(blk = integer(0)))
  expect_identical(x, dplyr::tibble(blk = integer(0), stream_name = character(0)))
})

test_that("fwa_add_stream_names_to_blk works missing values", {
  x <- fwa_add_stream_names_to_blk(data.frame(blk = NA_integer_))
  expect_identical(x, dplyr::tibble(blk = NA_integer_, stream_name = NA_character_))
})

test_that("fwa_add_stream_names_to_blk works custom", {
  x <- fwa_add_stream_names_to_blk(
    data.frame(blk = 356235759),
    data.frame(blk = 356235759, stream_name = "A Creek")
  )
  expect_identical(x, dplyr::tibble(
    blk = 356235759,
    stream_name = "A Creek"
  ))
})

test_that("fwa_add_stream_names_to_blk removes duplicates custom", {
  stream_name <- data.frame(
    blk = c(356235759, 356235759, 356235759),
    stream_name = c("A Creek", "A Creek", "A Different Creek")
  )

  x <- fwa_add_stream_names_to_blk(data.frame(blk = 356235759),
    stream_name = stream_name
  )

  expect_identical(x, dplyr::tibble(
    blk = c(356235759, 356235759),
    stream_name = c("A Creek", "A Different Creek")
  ))
})

test_that("fwa_add_stream_names_to_blk deals with missing blk", {
  stream_name <- data.frame(
    blk = 356235759,
    stream_name = "A Creek"
  )

  x <- fwa_add_stream_names_to_blk(data.frame(blk = 356235760),
    stream_name = stream_name
  )

  expect_identical(x, dplyr::tibble(
    blk = 356235760,
    stream_name = NA_character_
  ))
})
