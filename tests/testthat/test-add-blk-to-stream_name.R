test_that("fwa_add_blk_to_stream_name works simple example ", {
  blks <- fwa_add_blk_to_stream_name(data.frame(stream_name = "Sangan River"))
  expect_snapshot_data(blks, "simple")
})

test_that("fwa_add_blk_to_stream_name works no rows", {
  blks <- fwa_add_blk_to_stream_name(data.frame(stream_name = character(0)))
  expect_identical(nrow(blks), 0L)
  expect_identical(colnames(blks), c("stream_name", "blk"))
  expect_type(blks$blk, "integer")
})

test_that("fwa_add_blk_to_stream_name works all missing values", {
  blks <- fwa_add_blk_to_stream_name(data.frame(stream_name = NA_character_))
  expect_identical(nrow(blks), 1L)
  expect_identical(colnames(blks), c("stream_name", "blk"))
  expect_identical(blks$blk, NA_integer_)
})

test_that("fwa_add_blk_to_stream_name works multiple matches example ", {
  blks <- fwa_add_blk_to_stream_name(data.frame(stream_name = "Steep Creek"))
  expect_snapshot_data(blks, "multiple")
})


test_that("fwa_add_blk_to_stream_name works mix missing and present ", {
  blks <- fwa_add_blk_to_stream_name(data.frame(stream_name = c("Sangan River" , NA)))
  expect_snapshot_data(blks, "missing and present")
})



