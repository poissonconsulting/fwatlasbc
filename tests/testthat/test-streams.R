test_that("fwa_search_streams", {
  expect_identical(fwa_search_streams("sangan"), "Sangan River")
  expect_identical(fwa_search_streams("sangan", ignore_case = FALSE), character(0))
  expect_identical(fwa_search_streams("sangan2"), character(0))
  expect_identical(fwa_search_streams("3 Mile Creek"), "3 Mile Creek")
  expect_identical(fwa_search_streams("adams"), c("Adams Creek", "Adams River", "Marsh Adams Creek"))
})

test_that("fwa_stream_to_blue_line_keys", {
  expect_identical(nrow(fwa_streams_to_blue_line_keys("sangan")), 0L)
  x <- fwa_streams_to_blue_line_keys("Sangan River")
  expect_is(x, "data.frame")
  expect_identical(names(x), c("gnis_name", "blue_line_key", "watershed_group_code"))
  expect_identical(x$blue_line_key, 360879896L)

  x <- fwa_streams_to_blue_line_keys("3 Mile Creek")
  expect_identical(x$blue_line_key, c(356328957L, 359538484L))
})
