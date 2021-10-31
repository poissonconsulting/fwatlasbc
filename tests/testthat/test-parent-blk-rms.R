test_that("parent_blk_rms works", {
  rms <- data.frame(blk = 10, parent_blk = 2)
  expect_identical(fwa_parent_blk_rms(10, rms), 2L)
})

test_that("parent_blk_rms works unrooted", {
  rms <- data.frame(blk = 10, parent_blk = NA_integer_)
  expect_identical(fwa_parent_blk_rms(10, rms), NA_integer_)
})

test_that("parent_blk_rms works missing", {
  rms <- data.frame(blk = 10, parent_blk = 2)
  expect_identical(fwa_parent_blk_rms(11, rms), NA_integer_)
})

test_that("parent_blk_rms works zero length", {
  rms <- data.frame(blk = 10, parent_blk = 2)
  expect_identical(fwa_parent_blk_rms(numeric(0), rms), integer(0))
})

test_that("parent_blk_rms works called twice", {
  rms <- data.frame(blk = 10, parent_blk = 2)
  expect_identical(fwa_parent_blk_rms(c(10, 10), rms), c(2L, 2L))
})

test_that("parent_blk_rms errors if more than one parent_blk", {
  rms <- data.frame(blk = c(10, 10), parent_blk = c(2,3))
  chk::expect_chk_error(fwa_parent_blk_rms(10, rms))
})
