test_that("fwa_get_stream_name works simple case", {
  expect_identical(fwa_get_stream_name("sangan"),
                   dplyr::tibble(StreamName = "Sangan River"))
})

test_that("fwa_get_stream_name no rows with non-match", {
  expect_identical(fwa_get_stream_name("sangan2", ignore_case = FALSE),
                   dplyr::tibble(StreamName = character(0)))
})

test_that("fwa_get_stream_name ignore case works", {
  expect_identical(fwa_get_stream_name("sangan", ignore_case = FALSE),
                   dplyr::tibble(StreamName = character(0)))
})

test_that("fwa_get_stream_name works if same", {
  expect_identical(fwa_get_stream_name("3 Mile Creek"),
                   dplyr::tibble(StreamName = "3 Mile Creek"))
})

test_that("fwa_get_stream_name deals with multiple matches", {
  expect_identical(fwa_get_stream_name("adams"),
                   dplyr::tibble(StreamName = c("Adams Creek", "Adams River", "Marsh Adams Creek")))
})
