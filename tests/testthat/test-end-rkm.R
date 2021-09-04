test_that("fwa_add_end_id_to_rkm errors if no columns", {
  rlang::scoped_options(lifecycle_verbosity = "quiet")

  rkm <- data.frame(blue_line_key = 1L, rkm = seq(1, 10, by = 1))
  x <- data.frame(blue_line_key = 1L, rkm = c(3, 7.5, 9))

  chk::expect_chk_error(fwa_add_end_id_to_rkm(rkm, x))
})

test_that("fwa_add_end_id_to_rkm reorders", {
  rlang::scoped_options(lifecycle_verbosity = "quiet")

  rkm <- data.frame(blue_line_key = 1L, rkm = seq(1, 10, by = 1))
  x <- data.frame(blue_line_key = 1L, rkm = c(3, 7.5, 9), id = 3:1)

  erkm <- rkm
  rkm <- rkm[rev(order(rkm$rkm)),]

  expect_identical(dplyr::select(fwa_add_end_id_to_rkm(rkm, x), blue_line_key, rkm), dplyr::as_tibble(erkm))
})

test_that("fwa_add_end_id_to_rkm adds missing values if zero length", {
  rlang::scoped_options(lifecycle_verbosity = "quiet")

  rkm <- data.frame(blue_line_key = 1L, rkm = seq(1, 10, by = 1))
  x <- data.frame(blue_line_key = 1L, rkm = c(3, 7.5, 9), new = 2L)
  rkm$new <- NA_real_
  expect_identical(fwa_add_end_id_to_rkm(rkm, x[0,], id = "new"),
                   dplyr::mutate(dplyr::as_tibble(rkm), new = NA_integer_))
})

test_that("fwa_add_end_id_to_rkm adds zero length", {
  rlang::scoped_options(lifecycle_verbosity = "quiet")

  rkm <- data.frame(blue_line_key = 1L, rkm = seq(1, 10, by = 1))
  x <- data.frame(blue_line_key = 1L, rkm = c(3, 7.5, 9), new = 2L)

  erkm <- rkm[0,]
  erkm$new <- erkm$new <- numeric(0)
  expect_equal(fwa_add_end_id_to_rkm(rkm[0,], x, id = "new"), dplyr::as_tibble(erkm))
})

test_that("fwa_add_end_id_to_rkm simple example", {
  rlang::scoped_options(lifecycle_verbosity = "quiet")

  rkm <- data.frame(blue_line_key = 1L, rkm = seq(1, 10, by = 1))
  y <- data.frame(blue_line_key = 1L, rkm = c(3, 7.5, 9), new = c(3L, 7L, 10L))

  erkm <- rkm
  erkm$new <- c(3L, 3L, 3L, 7L, 7L, 7L, 7L, 10L, 10L, NA)
  expect_identical(fwa_add_end_id_to_rkm(rkm, y, id = "new"), dplyr::as_tibble(erkm))
})

test_that("fwa_add_end_id_to_rkm with missing values", {
  rlang::scoped_options(lifecycle_verbosity = "quiet")

  rkm <- data.frame(blue_line_key = 1L, rkm = seq(1, 10, by = 1))
  y <- data.frame(blue_line_key = 1L, rkm = c(2, 3, 7.5, 9), id = c(NA, 3L, 7L, 10L))

  erkm <- rkm
  erkm$id <- c(NA, NA, 3L, 7L, 7L, 7L, 7L, 10L, 10L, NA)
  expect_identical(fwa_add_end_id_to_rkm(rkm, y), dplyr::as_tibble(erkm))
})

test_that("fwa_add_end_id_to_rkm doesn't grab extra column", {
  rlang::scoped_options(lifecycle_verbosity = "quiet")

  rkm <- data.frame(blue_line_key = 1L, rkm = seq(1, 10, by = 1))
  y <- data.frame(blue_line_key = 1L, rkm = c(3, 7.5, 9), new = c(3L, 7L, 10L),
                  extra = c("x", "y", "x"))

  erkm <- rkm
  erkm$new <- c(3L, 3L, 3L, 7L, 7L, 7L, 7L, 10L, 10L, NA)
  expect_identical(fwa_add_end_id_to_rkm(rkm, y, id = "new"), dplyr::as_tibble(erkm))
})

test_that("fwa_add_end_id_to_rkm can handle non-integer columns", {
  rlang::scoped_options(lifecycle_verbosity = "quiet")

  rkm <- data.frame(blue_line_key = 1L, rkm = seq(1, 10, by = 1))
  y <- data.frame(blue_line_key = 1L, rkm = c(3, 7.5, 9), new = c("3", "7", "10"),
                  extra = c("x", "y", "x"))

  erkm <- rkm
  erkm$new <- c("3", "3", "3", "7", "7", "7", "7", "10", "10", NA)
  expect_identical(fwa_add_end_id_to_rkm(rkm, y, id = "new"), dplyr::as_tibble(erkm))
})

