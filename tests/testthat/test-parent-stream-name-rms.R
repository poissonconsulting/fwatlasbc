test_that("parent_stream_name works", {
  rms <- data.frame(blk = 10, parent_blk = 2)
  stream_name <- data.frame(blk = c(10, 2), stream_name = c("Sub Creek", "Super Creek"))
  expect_identical(fwa_parent_stream_name_rms("Sub Creek", rms, stream_name), "Super Creek")
})

test_that("parent_stream_name works no length", {
  rms <- data.frame(blk = 10, parent_blk = 2)
  stream_name <- data.frame(blk = c(10, 2), stream_name = c("Sub Creek", "Super Creek"))
  expect_identical(fwa_parent_stream_name_rms(character(0), rms, stream_name), character(0))
})

test_that("parent_stream_name works missing values", {
  rms <- data.frame(blk = 10, parent_blk = 2)
  stream_name <- data.frame(blk = c(10, 2), stream_name = c("Sub Creek", "Super Creek"))
  expect_identical(fwa_parent_stream_name_rms(NA_character_, rms, stream_name), NA_character_)
})

test_that("parent_stream_name works multiple", {
  rms <- data.frame(blk = 10, parent_blk = 2)
  stream_name <- data.frame(blk = c(10, 2), stream_name = c("Sub Creek", "Super Creek"))
  expect_identical(fwa_parent_stream_name_rms(c("Sub Creek", "Sub Creek"), rms, stream_name), c("Super Creek", "Super Creek"))
})

test_that("parent_stream_name works absent", {
  rms <- data.frame(blk = 10, parent_blk = 2)
  stream_name <- data.frame(blk = c(10, 2), stream_name = c("Sub Creek", "Super Creek"))
  expect_identical(fwa_parent_stream_name_rms("Not a creek", rms, stream_name), NA_character_)
})

test_that("parent_stream_name works if unrooted", {
  rms <- data.frame(blk = 10, parent_blk = 2)
  stream_name <- data.frame(blk = c(10, 2), stream_name = c("Sub Creek", "Super Creek"))
  expect_identical(fwa_parent_stream_name_rms("Super Creek", rms, stream_name), NA_character_)
})

test_that("parent_stream_name errors if multiple in child stream name", {
  rms <- data.frame(blk = 10, parent_blk = 2)
  stream_name <- data.frame(blk = c(10, 11, 2), stream_name = c("Sub Creek", "Sub Creek", "Super Creek"))
  chk::expect_chk_error(fwa_parent_stream_name_rms("Sub Creek", rms, stream_name), "value")
})

test_that("parent_stream_name handles multiple in parent stream name", {
  rms <- data.frame(blk = 10, parent_blk = 2)
  stream_name <- data.frame(blk = c(10, 11, 2), stream_name = c("Sub Creek", "Super Creek", "Super Creek"))
  expect_identical(fwa_parent_stream_name_rms("Sub Creek", rms, stream_name), "Super Creek")
})
