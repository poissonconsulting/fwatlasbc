test_that("fwa_prune_rms simple example", {
  x <- data.frame(blk = 1L, rm = seq(1000L, 10000L, by = 1000L),
                  parent_blk = NA_integer_, parent_rm = NA_integer_)
  y <- data.frame(blk = 1L, rm = 3000L)

  x <- fwa_prune_rms(x, y)
  expect_s3_class(x, "tbl")
  expect_identical(colnames(x), c("blk", "rm", "parent_blk", "parent_rm"))
  expect_identical(nrow(x), 2L)
  expect_identical(x$rm, c(1000L, 2000L))
  expect_snapshot_data(x, "simple")
})

test_that("fwa_prune_rms simple example all", {
  x <- dplyr::tribble(
    ~blk, ~rm, ~parent_blk, ~parent_rm,
    1,     0,    NA_integer_,   NA_integer_,
    1,      1,    NA_integer_,   NA_integer_,
    2,     0,    1,             1,
    2,     1,    1,             1,
    3,     0,    2,             1.5,
    3,     1,    2,             1.5,
    4,     0,    1,             0,
    4,     1,    1,             0)

  x <- fwa_prune_rms(x, data.frame(blk = 1, rm = 0, name = "new2"))

  expect_s3_class(x, "tbl")
  expect_identical(colnames(x), c("blk", "rm", "parent_blk", "parent_rm"))
  expect_identical(nrow(x), 0L)
})

test_that("fwa_prune_rms simple example partway up main", {
  x <- dplyr::tribble(
    ~blk, ~rm, ~parent_blk, ~parent_rm,
    1,     0,    NA_integer_,   NA_integer_,
    1,      1,    NA_integer_,   NA_integer_,
    2,     0,    1,             1,
    2,     1,    1,             1,
    3,     0,    2,             1.5,
    3,     1,    2,             1.5,
    4,     0,    1,             0,
    4,     1,    1,             0)

  x <- fwa_prune_rms(x, data.frame(blk = 1, rm = 1, name = "new2"))

  expect_s3_class(x, "tbl")
  expect_identical(colnames(x), c("blk", "rm", "parent_blk", "parent_rm"))
  expect_identical(nrow(x), 3L)
  expect_snapshot_data(x, "part")
})

test_that("fwa_prune_rms on trib", {
  x <- dplyr::tribble(
    ~blk, ~rm, ~parent_blk, ~parent_rm,
    1,     0,    NA_integer_,   NA_integer_,
    1,      1,    NA_integer_,   NA_integer_,
    2,     0,    1,             1,
    2,     1,    1,             1,
    3,     0,    2,             1.5,
    3,     1,    2,             1.5,
    4,     0,    1,             0,
    4,     1,    1,             0)

  x <- fwa_prune_rms(x, data.frame(blk = 2, rm = 1, name = "new2"))

  expect_s3_class(x, "tbl")
  expect_identical(colnames(x), c("blk", "rm", "parent_blk", "parent_rm"))
  expect_identical(nrow(x), 5L)
  expect_snapshot_data(x, "trib")
})


test_that("fwa_prune_rms off top", {
  x <- dplyr::tribble(
    ~blk, ~rm, ~parent_blk, ~parent_rm,
    1,     0,    NA_integer_,   NA_integer_,
    1,      1,    NA_integer_,   NA_integer_,
    2,     0,    1,             1,
    2,     1,    1,             1,
    3,     0,    2,             1.5,
    3,     1,    2,             1.5,
    4,     0,    1,             0,
    4,     1,    1,             0)

  x <- fwa_prune_rms(x, data.frame(blk = 1, rm = 10))

  expect_s3_class(x, "tbl")
  expect_identical(colnames(x), c("blk", "rm", "parent_blk", "parent_rm"))
  expect_identical(nrow(x), 8L)
  expect_snapshot_data(x, "offtop")
})

test_that("fwa_prune_rms two splits", {
  x <- dplyr::tribble(
    ~blk, ~rm, ~parent_blk, ~parent_rm,
    1,     0,    NA_integer_,   NA_integer_,
    1,      1,    NA_integer_,   NA_integer_,
    2,     0,    1,             1,
    2,     1,    1,             1,
    3,     0,    2,             1.5,
    3,     1,    2,             1.5,
    4,     0,    1,             0,
    4,     1,    1,             0)

  y <- data.frame(blk = c(1,2), rm = c(1,1), name = c("new2", "new1"))
  x <- fwa_prune_rms(x, y)

  expect_s3_class(x, "tbl")
  expect_identical(colnames(x), c("blk", "rm", "parent_blk", "parent_rm"))
  expect_identical(nrow(x), 5L)
  expect_snapshot_data(x, "twoprune")
})
