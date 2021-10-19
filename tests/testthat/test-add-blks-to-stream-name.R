test_that("fwa_add_blks_to_stream_name works simple example ", {
  blk <- fwa_add_blks_to_stream_name(data.frame(stream_name = "Sangan River"))
  expect_identical(blk, dplyr::tibble(stream_name = "Sangan River", blk = 360879896L))
})

test_that("fwa_add_blks_to_stream_name gives missing value misspecified name", {
  blk <- fwa_add_blks_to_stream_name(data.frame(stream_name = "Sangan2"))
  expect_identical(blk, dplyr::tibble(stream_name = "Sangan2", blk = NA_integer_))
})

test_that("fwa_add_blks_to_stream_name works no rows", {
  blk <- fwa_add_blks_to_stream_name(data.frame(stream_name = character(0)))
  expect_identical(blk, dplyr::tibble(stream_name = character(0), blk = integer(0)))
})

test_that("fwa_add_blks_to_stream_name works all missing values", {
  blk <- fwa_add_blks_to_stream_name(data.frame(stream_name = NA_character_))
  expect_identical(blk, dplyr::tibble(stream_name = NA_character_, blk = NA_integer_))
})

test_that("fwa_add_blks_to_stream_name works multiple matches example ", {
  blk <- fwa_add_blks_to_stream_name(data.frame(stream_name = "Steep Creek"))
  expect_identical(blk, dplyr::tibble(stream_name = c("Steep Creek", "Steep Creek", "Steep Creek"
  ), blk = c(356362258L, 356534225L, 356570155L)))
})

test_that("fwa_add_blks_to_stream_name preserves order", {
  blk <- fwa_add_blks_to_stream_name(data.frame(stream_name = c("Zymoetz River", "Aaltanhash River")))
  expect_identical(blk, dplyr::tibble(stream_name = c("Zymoetz River", "Aaltanhash River"
  ), blk = c(360881231L, 360886335L)))
})

test_that("fwa_add_blks_to_stream_name works mix multiple, missing and order ", {
  blk <- fwa_add_blks_to_stream_name(data.frame(stream_name = c("Zymoetz River", NA, "Steep Creek", "Sangan2")))
  expect_identical(blk, dplyr::tibble(stream_name = c("Zymoetz River", NA, "Steep Creek",
                                                "Steep Creek", "Steep Creek", "Sangan2"),
                                 blk = c(360881231L, NA, 356362258L,
                                         356534225L, 356570155L, NA)))
})

test_that("fwa_add_blks_to_stream_name errors existing blk", {
  expect_error(fwa_add_blks_to_stream_name(data.frame(stream_name = "Sangan River", blk = 1L)))
})
