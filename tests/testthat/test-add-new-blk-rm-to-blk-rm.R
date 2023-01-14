test_that("fwa_add_new_blk_to_blk_rm works", {
  rlang::local_options(nocache = TRUE)

  rm <- fwa_add_rms_to_blk(data.frame(blk = 356308001))

  x <- rm[rm$rm %in% c(0, 2000, 5000, 6000, 7000),]
  rm <- rm[rm$rm %in% c(0, 2000, 5000, 6000, 7000, 10000),]
  x <- fwa_snap_rms_to_rms(x, rm)
  rm <- x$rm
  x <- x$x

  rm <- x

  x <- x |>
    dplyr::slice(-1) |>
    dplyr::select(blk, rm, elevation)

  x <- fwa_add_new_blk_rm_to_blk_rm(x, rm)

  expect_s3_class(x, "sf")
  expect_identical(colnames(x), c("blk", "rm", "new_blk", "new_rm", "elevation", "geometry"))
  expect_equal(x$blk, rep(356308001, 4))
  expect_identical(x$new_blk, x$blk)
  expect_equal(x$rm, c(2000, 5000, 6000, 7000))
  expect_equal(x$new_rm, c(2000, 5000, 6000, 7000))
  expect_s3_class(x$geometry, "sfc_POINT")
})

test_that("fwa_add_new_blk_to_blk_rm works tibble and missing values", {
  rlang::local_options(nocache = TRUE)

  rm <- fwa_add_rms_to_blk(data.frame(blk = 356308001))

  x <- rm[rm$rm %in% c(0, 2000, 5000, 6000, 7000),]
  rm <- rm[rm$rm %in% c(0, 2000, 5000, 6000, 7000, 10000),]
  x <- fwa_snap_rms_to_rms(x, rm)
  rm <- x$rm
  x <- x$x

  rm <- x |>
    dplyr::slice(-2)

  x <- x |>
    dplyr::as_tibble() |>
    dplyr::select(blk, rm, elevation)

  x <- fwa_add_new_blk_rm_to_blk_rm(x, rm)

  expect_s3_class(x, "tbl_df")
  expect_identical(colnames(x), c("blk", "rm", "new_blk", "new_rm", "elevation"))
  expect_equal(x$blk, rep(356308001, 5))
  expect_equal(x$new_blk, c(356308001, NA, rep(356308001, 3)))
  expect_equal(x$rm, c(0, 2000, 5000, 6000, 7000))
  expect_equal(x$new_rm, c(0, NA, 5000, 6000, 7000))
})

