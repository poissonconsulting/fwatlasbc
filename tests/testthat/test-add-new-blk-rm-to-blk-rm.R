test_that("fwa_add_new_blk_to_blk_rm works", {
  rlang::local_options(nocache = TRUE)

  rm <- fwa_add_rms_to_blk(data.frame(blk = 356308001))

  x <- rm[rm$rm %in% c(0, 2000, 5000, 6000, 7000), ]
  rm <- rm[rm$rm %in% c(0, 2000, 5000, 6000, 7000, 10000), ]
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

  x <- rm[rm$rm %in% c(0, 2000, 5000, 6000, 7000), ]
  rm <- rm[rm$rm %in% c(0, 2000, 5000, 6000, 7000, 10000), ]
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

test_that("fwa_add_new_blk_to_blk_rm works different names", {
  rlang::local_options(nocache = TRUE)

  rm <- fwa_add_rms_to_blk(data.frame(blk = 356308001))

  x <- rm[rm$rm %in% c(0, 2000, 5000, 6000, 7000), ]
  rm <- rm[rm$rm %in% c(0, 2000, 5000, 6000, 7000, 10000), ]
  x <- fwa_snap_rms_to_rms(x, rm)
  rm <- x$rm
  x <- x$x

  rm <- x |>
    dplyr::slice(-2) |>
    dplyr::rename(
      rm2 = rm, blk2 = blk,
      old_rm = new_rm, old_blk = new_blk
    )

  x <- x |>
    dplyr::as_tibble() |>
    dplyr::select(blk, rm, elevation) |>
    dplyr::rename(rm1 = rm, blk1 = blk)


  x <- fwa_add_new_blk_rm_to_blk_rm(x, rm,
    rm = "rm1", blk = "blk1",
    rm2 = "rm2", blk2 = "blk2",
    new_rm = "old_rm", new_blk = "old_blk"
  )

  expect_s3_class(x, "tbl_df")
  expect_identical(colnames(x), c("blk1", "rm1", "old_blk", "old_rm", "elevation"))
  expect_equal(x$blk1, rep(356308001, 5))
  expect_equal(x$old_blk, c(356308001, NA, rep(356308001, 3)))
  expect_equal(x$rm1, c(0, 2000, 5000, 6000, 7000))
  expect_equal(x$old_rm, c(0, NA, 5000, 6000, 7000))
})


test_that("fwa_add_new_blk_to_blk_rm works same blk2", {
  rlang::local_options(nocache = TRUE)

  rm <- fwa_add_rms_to_blk(data.frame(blk = 356308001))

  x <- rm[rm$rm %in% c(0, 2000, 5000, 6000, 7000), ]
  rm <- rm[rm$rm %in% c(0, 2000, 5000, 6000, 7000, 10000), ]
  x <- fwa_snap_rms_to_rms(x, rm)
  rm <- x$rm
  x <- x$x

  rm <- x |>
    dplyr::slice(-2)

  x <- x |>
    dplyr::as_tibble() |>
    dplyr::select(blk, rm, elevation)


  x <- fwa_add_new_blk_rm_to_blk_rm(x, rm, new_blk = "blk")

  expect_s3_class(x, "tbl_df")
  expect_identical(colnames(x), c("blk", "rm", "new_rm", "elevation"))
  expect_equal(x$blk, rep(356308001, 5))
  expect_equal(x$rm, c(0, 2000, 5000, 6000, 7000))
  expect_equal(x$new_rm, c(0, NA, 5000, 6000, 7000))
})

test_that("fwa_add_new_blk_to_blk_rm works new_blk = NULL", {
  rlang::local_options(nocache = TRUE)

  rm <- fwa_add_rms_to_blk(data.frame(blk = 356308001))

  x <- rm[rm$rm %in% c(0, 2000, 5000, 6000, 7000), ]
  rm <- rm[rm$rm %in% c(0, 2000, 5000, 6000, 7000, 10000), ]
  x <- fwa_snap_rms_to_rms(x, rm)
  rm <- x$rm
  x <- x$x

  rm <- x |>
    dplyr::slice(-2)

  x <- x |>
    dplyr::as_tibble() |>
    dplyr::select(blk, rm, elevation)


  x <- fwa_add_new_blk_rm_to_blk_rm(x, rm, new_blk = NULL)

  expect_s3_class(x, "tbl_df")
  expect_identical(colnames(x), c("blk", "rm", "new_rm", "elevation"))
  expect_equal(x$blk, rep(356308001, 5))
  expect_equal(x$rm, c(0, 2000, 5000, 6000, 7000))
  expect_equal(x$new_rm, c(0, NA, 5000, 6000, 7000))
})

test_that("fwa_add_new_blk_to_blk_rm works rename", {
  rlang::local_options(nocache = TRUE)

  rm <- fwa_add_rms_to_blk(data.frame(blk = 356308001))

  x <- rm[rm$rm %in% c(0, 2000, 5000, 6000, 7000), ]
  rm <- rm[rm$rm %in% c(0, 2000, 5000, 6000, 7000, 10000), ]
  x <- fwa_snap_rms_to_rms(x, rm)
  rm <- x$rm
  x <- x$x

  rm <- x |>
    dplyr::slice(-2) |>
    mutate(new_blk = new_blk + 1L)

  x <- x |>
    dplyr::as_tibble() |>
    dplyr::select(blk, rm, elevation)


  x <- fwa_add_new_blk_rm_to_blk_rm(x, rm,
    new_blk_to = "zz_blk",
    new_rm_to = "aa"
  )

  expect_s3_class(x, "tbl_df")
  expect_identical(colnames(x), c("blk", "rm", "zz_blk", "aa", "elevation"))
  expect_equal(x$blk, rep(356308001, 5))
  expect_equal(x$rm, c(0, 2000, 5000, 6000, 7000))
  expect_equal(x$zz_blk, c(356308002L, NA, rep(356308002L, 3)))
  expect_equal(x$aa, c(0, NA, 5000, 6000, 7000))
})
