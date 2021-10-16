# test_that("fwa_add_columns_to_rkm adds no columns", {
#   rlang::scoped_options(lifecycle_verbosity = "quiet")
#
#   rkm <- data.frame(blue_line_key = 1L, rkm = seq(1, 10, by = 1))
#   x <- data.frame(blue_line_key = 1L, rkm = c(3, 7.5, 9))
#
#   expect_identical(fwa_add_columns_to_rkm(rkm, x), dplyr::as_tibble(rkm))
# })
#
# test_that("fwa_add_columns_to_rkm reorders", {
#   rlang::scoped_options(lifecycle_verbosity = "quiet")
#
#   rkm <- data.frame(blue_line_key = 1L, rkm = seq(1, 10, by = 1))
#   x <- data.frame(blue_line_key = 1L, rkm = c(3, 7.5, 9))
#
#   erkm <- rkm
#   rkm <- rkm[rev(order(rkm$rkm)),]
#
#   expect_identical(fwa_add_columns_to_rkm(rkm, x), dplyr::as_tibble(erkm))
# })
#
# test_that("fwa_add_columns_to_rkm adds missing values if zero length", {
#   rlang::scoped_options(lifecycle_verbosity = "quiet")
#
#   rkm <- data.frame(blue_line_key = 1L, rkm = seq(1, 10, by = 1))
#   x <- data.frame(blue_line_key = 1L, rkm = c(3, 7.5, 9), new = 2)
#   rkm$new <- NA_real_
#   expect_identical(fwa_add_columns_to_rkm(rkm, x[0,]), dplyr::as_tibble(rkm))
# })
#
# test_that("fwa_add_columns_to_rkm adds zero length", {
#   rlang::scoped_options(lifecycle_verbosity = "quiet")
#
#   rkm <- data.frame(blue_line_key = 1L, rkm = seq(1, 10, by = 1))
#   x <- data.frame(blue_line_key = 1L, rkm = c(3, 7.5, 9), new = 2)
#
#   erkm <- rkm[0,]
#   erkm$new <- erkm$new <- numeric(0)
#   expect_identical(fwa_add_columns_to_rkm(rkm[0,], x), dplyr::as_tibble(erkm))
# })
#
# test_that("fwa_add_columns_to_rkm simple example", {
#   rlang::scoped_options(lifecycle_verbosity = "quiet")
#
#   rkm <- data.frame(blue_line_key = 1L, rkm = seq(1, 10, by = 1))
#   y <- data.frame(blue_line_key = 1L, rkm = c(3, 7.5, 9), new = c(3, 7.5, 10))
#
#   erkm <- rkm
#   erkm$new <- c(3, 3, 3, 7.5, 7.5, 7.5, 7.5, 10, 10, NA)
#   expect_identical(fwa_add_columns_to_rkm(rkm, y), dplyr::as_tibble(erkm))
# })
#
# test_that("fwa_add_columns_to_rkm with missing values", {
#   rlang::scoped_options(lifecycle_verbosity = "quiet")
#
#   rkm <- data.frame(blue_line_key = 1L, rkm = seq(1, 10, by = 1))
#   y <- data.frame(blue_line_key = 1L, rkm = c(2, 3, 7.5, 9), new = c(NA, 3, 7.5, 10))
#
#   erkm <- rkm
#   erkm$new <- c(NA, NA, 3, 7.5, 7.5, 7.5, 7.5, 10, 10, NA)
#   expect_identical(fwa_add_columns_to_rkm(rkm, y), dplyr::as_tibble(erkm))
# })
#
# test_that("fwa_add_columns_to_rkm with more than one column", {
#   rlang::scoped_options(lifecycle_verbosity = "quiet")
#
#   rkm <- data.frame(blue_line_key = 1L, rkm = seq(1, 10, by = 1))
#   y <- data.frame(blue_line_key = 1L, rkm = c(2, 3, 7.5, 9), new2 = c(10, 7.5, 3, NA),
#                   new = c(NA, 3, 7.5, 10))
#
#   erkm <- rkm
#   erkm$new2 <- c(10, 10, 7.5, 3, 3, 3, 3, NA, NA, NA)
#   erkm$new <- c(NA, NA, 3, 7.5, 7.5, 7.5, 7.5, 10, 10, NA)
#   expect_identical(fwa_add_columns_to_rkm(rkm, y), dplyr::as_tibble(erkm))
# })
