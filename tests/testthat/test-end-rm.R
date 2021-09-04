test_that("fwa_add_end_id_to_rm errors if no columns", {

  rm <- data.frame(blue_line_key = 1L, rm = seq(1000L, 10000L, by = 1000))
  x <- data.frame(blue_line_key = 1L, rm = c(3000L, 7500L, 9000L))

  chk::expect_chk_error(fwa_add_end_id_to_rm(rm, x))
})

test_that("fwa_add_end_id_to_rm reorders", {
  rm <- data.frame(blue_line_key = 1L, rm = seq(1000L, 10000L, by = 1000L))
  x <- data.frame(blue_line_key = 1L, rm = c(3000L, 7500L, 9000L), id = 3:1)

  erm <- rm
  rm <- rm[rev(order(rm$rm)),]

  expect_identical(dplyr::select(fwa_add_end_id_to_rm(rm, x), blue_line_key, rm), dplyr::as_tibble(erm))
})

test_that("fwa_add_end_id_to_rm adds missing values if zero length", {
  rm <- data.frame(blue_line_key = 1L, rm = seq(1000L, 10000L, by = 1000L))
  x <- data.frame(blue_line_key = 1L, rm = c(3000L, 7500L, 9000L), new = 2000L)
  rm$new <- NA_real_
  expect_identical(fwa_add_end_id_to_rm(rm, x[0,], id = "new"),
                   dplyr::mutate(dplyr::as_tibble(rm), new = NA_integer_))
})

test_that("fwa_add_end_id_to_rm adds zero length", {
  rm <- data.frame(blue_line_key = 1L, rm = seq(1000L, 10000L, by = 1000L))
  x <- data.frame(blue_line_key = 1L, rm = c(3000L, 7500L, 9000L), new = 2000L)

  erm <- rm[0,]
  erm$new <- erm$new <- numeric(0)
  expect_equal(fwa_add_end_id_to_rm(rm[0,], x, id = "new"), dplyr::as_tibble(erm))
})

test_that("fwa_add_end_id_to_rm simple example", {
  rm <- data.frame(blue_line_key = 1L, rm = seq(1000L, 10000L, by = 1000L))
  y <- data.frame(blue_line_key = 1L, rm = c(3000L, 7500L, 9000L), new = c(3L, 7L, 10L))

  erm <- rm
  erm$new <- c(3L, 3L, 3L, 7L, 7L, 7L, 7L, 10L, 10L, NA)
  expect_identical(fwa_add_end_id_to_rm(rm, y, id = "new"), dplyr::as_tibble(erm))
})

test_that("fwa_add_end_id_to_rm with missing values", {
  rm <- data.frame(blue_line_key = 1L, rm = seq(1000L, 10000L, by = 1000L))
  y <- data.frame(blue_line_key = 1L, rm = c(2000L, 3000L, 7500L, 9000L), id = c(NA, 3L, 7L, 10L))

  erm <- rm
  erm$id <- c(NA, NA, 3L, 7L, 7L, 7L, 7L, 10L, 10L, NA)
  expect_identical(fwa_add_end_id_to_rm(rm, y), dplyr::as_tibble(erm))
})

test_that("fwa_add_end_id_to_rm doesn't grab extra column", {
  rm <- data.frame(blue_line_key = 1L, rm = seq(1000L, 10000L, by = 1000L))
  y <- data.frame(blue_line_key = 1L, rm = c(3000L, 7500L, 9000L), new = c(3L, 7L, 10L),
                  extra = c("x", "y", "x"))

  erm <- rm
  erm$new <- c(3L, 3L, 3L, 7L, 7L, 7L, 7L, 10L, 10L, NA)
  expect_identical(fwa_add_end_id_to_rm(rm, y, id = "new"), dplyr::as_tibble(erm))
})

test_that("fwa_add_end_id_to_rm can handle non-integer columns", {
  rm <- data.frame(blue_line_key = 1L, rm = seq(1000L, 10000L, by = 1000L))
  y <- data.frame(blue_line_key = 1L, rm = c(3000L, 7500L, 9000L), new = c("3", "7", "10"),
                  extra = c("x", "y", "x"))

  erm <- rm
  erm$new <- c("3", "3", "3", "7", "7", "7", "7", "10", "10", NA)
  expect_identical(fwa_add_end_id_to_rm(rm, y, id = "new"), dplyr::as_tibble(erm))
})
