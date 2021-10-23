test_that("round_up", {
  expect_identical(round_up(0.4999999), 0L)
  expect_identical(round_up(0.5), 1L)
  expect_identical(round_up(1.4999999), 1L)
  expect_identical(round_up(1.5), 2L)
  expect_identical(round_up(2.4999999), 2L)
  expect_identical(round_up(2.5), 3L)
})
