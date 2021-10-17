test_that("fwa_add_blks_to_stream_name works simple example ", {
  blk <- fwa_add_blks_to_stream_name(data.frame(StreamName = "Sangan River"))
  expect_identical(blk, dplyr::tibble(StreamName = "Sangan River", BLK = 360879896L))
})

test_that("fwa_add_blks_to_stream_name gives missing value misspecified name", {
  blk <- fwa_add_blks_to_stream_name(data.frame(StreamName = "Sangan2"))
  expect_identical(blk, dplyr::tibble(StreamName = "Sangan2", BLK = NA_integer_))
})

test_that("fwa_add_blks_to_stream_name works no rows", {
  blk <- fwa_add_blks_to_stream_name(data.frame(StreamName = character(0)))
  expect_identical(blk, dplyr::tibble(StreamName = character(0), BLK = integer(0)))
})

test_that("fwa_add_blks_to_stream_name works all missing values", {
  blk <- fwa_add_blks_to_stream_name(data.frame(StreamName = NA_character_))
  expect_identical(blk, dplyr::tibble(StreamName = NA_character_, BLK = NA_integer_))
})

test_that("fwa_add_blks_to_stream_name works multiple matches example ", {
  blk <- fwa_add_blks_to_stream_name(data.frame(StreamName = "Steep Creek"))
  expect_identical(blk, dplyr::tibble(StreamName = c("Steep Creek", "Steep Creek", "Steep Creek"
  ), BLK = c(356362258L, 356534225L, 356570155L)))
})

test_that("fwa_add_blks_to_stream_name preserves order", {
  blk <- fwa_add_blks_to_stream_name(data.frame(StreamName = c("Zymoetz River", "Aaltanhash River")))
  expect_identical(blk, dplyr::tibble(StreamName = c("Zymoetz River", "Aaltanhash River"
  ), BLK = c(360881231L, 360886335L)))
})

test_that("fwa_add_blks_to_stream_name works mix multiple, missing and order ", {
  blk <- fwa_add_blks_to_stream_name(data.frame(StreamName = c("Zymoetz River", NA, "Steep Creek", "Sangan2")))
  expect_identical(blk, dplyr::tibble(StreamName = c("Zymoetz River", NA, "Steep Creek",
                                                "Steep Creek", "Steep Creek", "Sangan2"),
                                 BLK = c(360881231L, NA, 356362258L,
                                         356534225L, 356570155L, NA)))
})

test_that("fwa_add_blks_to_stream_name errors existing BLK", {
  expect_error(fwa_add_blks_to_stream_name(data.frame(StreamName = "Sangan River", BLK = 1L)))
})
