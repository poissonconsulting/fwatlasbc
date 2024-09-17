test_that("fwa_find_stream_names works simple case", {
  expect_identical(
    fwa_find_stream_names("sangan"),
    dplyr::tibble(stream_name = "Sangan River")
  )
})

test_that("fwa_find_stream_names no rows with non-match", {
  expect_identical(
    fwa_find_stream_names("sangan2", ignore_case = FALSE),
    dplyr::tibble(stream_name = character(0))
  )
})

test_that("fwa_find_stream_names ignore case works", {
  expect_identical(
    fwa_find_stream_names("sangan", ignore_case = FALSE),
    dplyr::tibble(stream_name = character(0))
  )
})

test_that("fwa_find_stream_names works if same", {
  expect_identical(
    fwa_find_stream_names("3 Mile Creek"),
    dplyr::tibble(stream_name = "3 Mile Creek")
  )
})

test_that("fwa_find_stream_names deals with multiple matches", {
  expect_identical(
    fwa_find_stream_names("adams"),
    dplyr::tibble(stream_name = c("Adams Creek", "Adams River", "Marsh Adams Creek"))
  )
})
