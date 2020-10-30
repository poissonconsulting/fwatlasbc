test_that("fwa_search_streams", {
  expect_identical(fwa_search_streams("sangan"), "Sangan River")
  expect_identical(fwa_search_streams("sangan", ignore_case = FALSE), character(0))
  expect_identical(fwa_search_streams("sangan2"), character(0))
  expect_identical(fwa_search_streams("3 Mile Creek"), "3 Mile Creek")
  expect_identical(fwa_search_streams("adams"), c("Adams Creek", "Adams River", "Marsh Adams Creek"))
})

test_that("fwa_stream_to_blue_line_keys", {
  expect_identical(fwa_stream_to_blue_line_keys("sangan"), integer(0))
  expect_identical(fwa_stream_to_blue_line_keys("Sangan River"), 360879896L)
  expect_identical(fwa_stream_to_blue_line_keys("3 Mile Creek"), c(356328957L, 359538484L))
})
