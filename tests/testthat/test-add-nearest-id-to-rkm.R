test_that("fwa_add_nearest_id_to_rkm", {
  rlang::scoped_options(lifecycle_verbosity = "quiet")

  x <- fwa_rkm(blue_line_key = 356308001L, interval = 100, start = 10000, limit = 10)
  expect_s3_class(x, "sf")

  x <- x[1:10,]
  y <- x[5:7,]

  chk::expect_chk_error(fwa_add_nearest_id_to_rkm(x, y))
  y$new <- c(3L,5L,2L)
  expect_identical(fwa_add_nearest_id_to_rkm(x, y[integer(0),], id = "new"),
                   dplyr::mutate(x, new = NA_integer_))
  expect_identical(fwa_add_nearest_id_to_rkm(x[integer(0),], y, id = "new"),
                   dplyr::mutate(x, new = NA_integer_)[integer(0),])
  expect_identical(fwa_add_nearest_id_to_rkm(x, dplyr::select(y, -blue_line_key) , id = "new"),
                   dplyr::mutate(x, new = c(NA, NA, NA, NA, 3L, 5L, 2L, NA, NA, NA)))
  expect_identical(fwa_add_nearest_id_to_rkm(x, y , id = "new"),
                   dplyr::mutate(x, new = c(NA, NA, NA, NA, 3L, 5L, 2L, NA, NA, NA)))
  expect_identical(fwa_add_nearest_id_to_rkm(x, dplyr::select(y, -blue_line_key) , id = "new", max_distance = 100),
                   dplyr::mutate(x, new = c(NA, NA, NA, 3L, 3L, 5L, 2L, 2L, NA, NA)))

  y$blue_line_key[1] <- 1L
  expect_identical(dplyr::select(fwa_add_nearest_id_to_rkm(x, y , id = "new", max_distance = 100), blue_line_key, rkm, new),
                   dplyr::select(dplyr::mutate(x, new = c(NA, NA, NA, NA, 5L, 5L, 2L, 2L, NA, NA)),
                                 blue_line_key, rkm, new))
  x$blue_line_key[4] <- 1L
  expect_identical(dplyr::select(fwa_add_nearest_id_to_rkm(x, y , id = "new", max_distance = 100), blue_line_key, rkm, new),
                   dplyr::select(dplyr::mutate(x, new = c(NA, NA, NA, 3L, 5L, 5L, 2L, 2L, NA, NA)),
                                 blue_line_key, rkm, new))
  x$blue_line_key[5] <- NA
  expect_identical(fwa_add_nearest_id_to_rkm(x, y , id = "new", max_distance = 100),
                   dplyr::mutate(x, new = c(NA, NA, NA, 3L, 3L, 5L, 2L, 2L, NA, NA)))
  expect_identical(fwa_add_nearest_id_to_rkm(x, y , id = "new", max_distance = 100, max_end_distance = 10),
                   dplyr::mutate(x, new = c(NA, NA, NA, 3L, 3L, 5L, 2L, NA, NA, NA)))

  # not drop blue line keys that unrecognized!
  x$blue_line_key[6] <- 2L
  expect_identical(fwa_add_nearest_id_to_rkm(x, y , id = "new", max_distance = 100, max_end_distance = 10),
                   dplyr::mutate(x, new = c(NA, NA, NA, 3L, 3L, NA, 2L, NA, NA, NA)))

  # handles non-integer id
  y$new <- as.character(y$new)
  expect_identical(fwa_add_nearest_id_to_rkm(x, y , id = "new", max_distance = 100, max_end_distance = 10),
                   dplyr::mutate(x, new = c(NA, NA, NA, "3", "3", NA, "2", NA, NA, NA)))
})
