test_that("fwa_swap_branches_rms works", {

  rm <- dplyr::tribble(
    ~blk, ~rm, ~parent_blk, ~parent_rm,
    1,     0,    NA_integer_,   NA_integer_,
    1,      1,    NA_integer_,   NA_integer_,
    1,      2,    NA_integer_,   NA_integer_,
    2,     0,    1,             1,
    2,     1,    1,             1,
    2,     2,    1,             1)

  x <- fwa_swap_branches_rms(rm, data.frame(blk = 2), adjust_points = FALSE)
  expect_identical(colnames(x), c("blk", "rm", "parent_blk", "parent_rm"))
  expect_identical(nrow(x), 6L)

  expect_snapshot_data(x, "simpleswap")
})

test_that("fwa_swap_branches_rms useful error message no points and adjust", {

  rm <- dplyr::tribble(
    ~blk, ~rm, ~parent_blk, ~parent_rm,
    1,     0,    NA_integer_,   NA_integer_,
    1,      1,    NA_integer_,   NA_integer_,
    2,     0,    1,             1)

  chk::expect_chk_error(fwa_swap_branches_rms(rm, data.frame(blk = 2)),
                   "`x` must inherit from S3 class 'sf'\\.$")
})

test_that("fwa_swap_branches_rms errors unrooted trib", {

  rm <- dplyr::tribble(
    ~blk, ~rm, ~parent_blk, ~parent_rm,
    1,     0,    NA_integer_,   NA_integer_,
    1,      1,    NA_integer_,   NA_integer_,
    2,     1,    1,             1)

  chk::expect_chk_error(fwa_swap_branches_rms(rm, data.frame(blk = 2), adjust_points = FALSE),
                        "^`blk` 2 from `x` is unrooted \\(missing rm == 0\\)\\.$")
})

test_that("fwa_swap_branches_rms works with a trib child", {

  rm <- dplyr::tribble(
    ~blk, ~rm, ~parent_blk, ~parent_rm,
    1,     0,    NA_integer_,   NA_integer_,
    1,      1,    NA_integer_,   NA_integer_,
    1,      2,    NA_integer_,   NA_integer_,
    2,     0,    1,             1,
    2,     1,    1,             1,
    2,     2,    1,             1,
    3,     0,    2,             1,
    3,     1,    2,             1)

  x <- fwa_swap_branches_rms(rm, data.frame(blk = 2), adjust_points = FALSE)
  expect_identical(colnames(x), c("blk", "rm", "parent_blk", "parent_rm"))
  expect_identical(nrow(x), 8L)

  expect_snapshot_data(x, "tribchild")
})

test_that("fwa_swap_branches_rms works with a main child", {

  rm <- dplyr::tribble(
    ~blk, ~rm, ~parent_blk, ~parent_rm,
    1,     0,    NA_integer_,   NA_integer_,
    1,      1,    NA_integer_,   NA_integer_,
    1,      2,    NA_integer_,   NA_integer_,
    2,     0,    1,             1,
    2,     1,    1,             1,
    2,     2,    1,             1,
    3,     0,    1,             2,
    3,     1,    1,             2)

  x <- fwa_swap_branches_rms(rm, data.frame(blk = 2), adjust_points = FALSE)
  expect_identical(colnames(x), c("blk", "rm", "parent_blk", "parent_rm"))
  expect_identical(nrow(x), 8L)

  expect_snapshot_data(x, "mainchild")
})

test_that("fwa_swap_branches_rms works with a trib child and part parent_rm", {

  rm <- dplyr::tribble(
    ~blk, ~rm, ~parent_blk, ~parent_rm,
    1,     0,    NA_integer_,   NA_integer_,
    1,      1,    NA_integer_,   NA_integer_,
    1,      2,    NA_integer_,   NA_integer_,
    1,      3,    NA_integer_,   NA_integer_,
    2,     0,    1,             0.5,
    2,     1,    1,             0.5,
    2,     2,    1,             0.5)


  x <- fwa_swap_branches_rms(rm, data.frame(blk = 2), adjust_points = FALSE)
  expect_identical(colnames(x), c("blk", "rm", "parent_blk", "parent_rm"))
  expect_identical(nrow(x), 7L)

  expect_snapshot_data(x, "tribchildpart")
})

test_that("fwa_swap_branches_rms works with a trib child and part parent_rm and adjust points", {

  rm <- dplyr::tribble(
    ~blk, ~rm, ~parent_blk, ~parent_rm,  ~x, ~y,
    1,     0,    NA_integer_,   NA_integer_, 0, 0,
    1,      1,    NA_integer_,   NA_integer_, 1, 0,
    1,      2,    NA_integer_,   NA_integer_, 2, 0,
    1,      3,    NA_integer_,   NA_integer_, 3, 0,
    2,     0,    1,             0.5,          0.5, 0,
    2,     1,    1,             0.5,          0.5, 1,
    2,     2,    1,             0.5,          0.5, 2) |>
    sf::st_as_sf(coords = c("x", "y"), dim = "XY")


  x <- fwa_swap_branches_rms(rm, data.frame(blk = 2))
  expect_s3_class(x, "sf")
  expect_identical(colnames(x), c("blk", "rm", "parent_blk", "parent_rm", "geometry"))
  expect_identical(nrow(x), 7L)

  skip("haven't adjusted points yet")
  expect_snapshot_data(x, "tribchildpartutms")
})
