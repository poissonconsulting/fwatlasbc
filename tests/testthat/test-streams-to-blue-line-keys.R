test_that("fwa_streams_to_blue_line_keys works one match", {
  rlang::scoped_options(lifecycle_verbosity = "quiet")

  blks <- fwa_streams_to_blue_line_keys("Sangan River")

  expect_s3_class(blks, "tbl_df")
  expect_identical(colnames(blks), c("stream_name", "blue_line_key"))
  expect_identical(blks$stream_name, "Sangan River")
  expect_identical(blks$blue_line_key, 360879896L)
})

test_that("fwa_streams_to_blue_line_keys works multiple matches", {
  rlang::scoped_options(lifecycle_verbosity = "quiet")

  blks <- fwa_streams_to_blue_line_keys("3 Mile Creek")

  expect_s3_class(blks, "tbl_df")
  expect_identical(colnames(blks), c("stream_name", "blue_line_key"))
  expect_identical(blks$stream_name, rep("3 Mile Creek", 2L))
  expect_identical(blks$blue_line_key, c(356328957L, 359538484L))
})

test_that("fwa_streams_to_blue_line_keys works no matches", {
  rlang::scoped_options(lifecycle_verbosity = "quiet")

  blks <- fwa_streams_to_blue_line_keys("not a stream")
  expect_s3_class(blks, "tbl_df")
  expect_identical(colnames(blks), c("stream_name", "blue_line_key"))
  expect_identical(nrow(blks), 0L)
  expect_identical(blks$stream_name, character(0))
  expect_identical(blks$blue_line_key, integer(0))
})
