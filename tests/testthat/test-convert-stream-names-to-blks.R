test_that("fwa_convert_stream_names_to_blks works", {
  expect_identical(fwa_convert_stream_names_to_blks("a name"), 576524986L)
  expect_identical(fwa_convert_stream_names_to_blks("a name2"), 151860402L)
  expect_identical(
    fwa_convert_stream_names_to_blks(c("a name", "a name2")),
    c(576524986L, 151860402L)
  )
  expect_identical(
    fwa_convert_stream_names_to_blks(c("a name3", "a name3")),
    c(597060036L, 597060036L)
  )
})

test_that("fwa_convert_stream_names_to_blks errors", {
  expect_error(
    fwa_convert_stream_names_to_blks(597060036L),
    "^`names` must be character or factor or NULL\\.$"
  )
})

test_that("fwa_convert_stream_names_to_blks factors", {
  expect_identical(fwa_convert_stream_names_to_blks(factor("a name")), 576524986L)
  expect_identical(fwa_convert_stream_names_to_blks(factor("a name", levels = c("a name0", "a name"))), 576524986L)
})

test_that("fwa_convert_stream_names_to_blks null", {
  expect_identical(fwa_convert_stream_names_to_blks(NULL), integer())
})

test_that("fwa_convert_stream_names_to_blks missing values", {
  expect_identical(fwa_convert_stream_names_to_blks(character()), integer())
  expect_identical(fwa_convert_stream_names_to_blks(NA_character_), NA_integer_)
  expect_identical(
    fwa_convert_stream_names_to_blks(c("a name2", NA_integer_)),
    c(151860402L, NA_integer_)
  )
})
