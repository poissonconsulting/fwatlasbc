test_that("fwa_add_split_to_rms simple example", {
  x <- data.frame(blk = 1L, rm = seq(1000L, 10000L, by = 1000L),
                  parent_blk = NA_integer_, parent_rm = NA_integer_)
  y <- data.frame(blk = 1L, rm = 3000L, name = "new")

  x <- fwa_add_split_to_rms(x, y)
  expect_s3_class(x, "tbl")
  expect_identical(colnames(x), c("blk", "rm", "parent_blk", "parent_rm", "new"))
  expect_identical(nrow(x), 10L)
  expect_identical(x$new, c(rep(FALSE, 2), rep(TRUE, 8)))
  expect_snapshot_data(x, "simple")
})
