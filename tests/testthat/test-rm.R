test_that("fwa_rm works", {
  x <- fwa_rm(blue_line_key = 356308001L, interval = 100, start = 10000)
  expect_s3_class(x, "sf")
  expect_identical(names(x), c("blue_line_key", "rm", "geometry"))
  expect_type(x$blue_line_key, "integer")
  expect_equal(x$rm, seq(10000L, 19200L, by = 100L))
})
