test_that("rm functions work", {
  x <- fwa_rm(blue_line_key = 356308001L, interval = 100, start = 10000)
  expect_is(x, "sf")
  expect_identical(names(x), c("blue_line_key", "rm", "geometry"))
  expect_is(x$blue_line_key, "integer")
  expect_equal(x$rm, seq(10000L, 19200L, by = 100L))
})

test_that("fwa_nearest_rm works", {
  rm <- fwa_rm(blue_line_key = 356308001, interval = 1000)

  x <- rm[rm$rm %in% c(0, 2000, 5000, 6000, 7000),]
  rm <- rm[!rm$rm %in% c(0, 2000, 5000, 6000, 7000),]
  y <- fwa_nearest_rm(x, rm)
  ey <- x
  ey$rm <- c(1000,3000,4000,4000,8000)
  ey$distance_to_rm <- c(873.50885850392, 535.63765010454, 754.230245890789, 610.731097004499,
                          514.952511361304)
  expect_equal(y, ey)

  rm$blue_line_key[rm$rm == 8000] <- 2L
  x2 <- x
  x2$blue_line_key[1:2] <- NA_integer_
  y <- fwa_nearest_rm(x2, rm)
  ey <- x
  ey$rm <- c(1000,3000,4000,4000,9000)
  ey$distance_to_rm <- c(873.50885850392, 535.63765010454, 754.230245890789, 610.731097004499,
                          645.410842257104)
  expect_equal(y, ey[c("blue_line_key", "rm", "distance_to_rm")])

  x2 <- sf::st_zm(x2)
  is.na(x2$geometry[x2$rm == 6000]) <- TRUE
  y <- fwa_nearest_rm(x2, rm)
  ey <- sf::st_zm(x)
  is.na(ey$geometry[ey$rm == 6000]) <- TRUE
  ey$rm <- c(1000,3000,4000,NA,9000)
  ey$distance_to_rm <- c(873.50885850392, 535.63765010454, 754.230245890789, NA,
                          645.410842257104)
  y

  expect_equal(y, ey[c("blue_line_key", "rm", "distance_to_rm")])

  is.na(x2$blue_line_key) <- TRUE
  y <- fwa_nearest_rm(x2, rm)

  expect_identical(y$blue_line_key, c(356308001L, 356308001L, 356308001L, NA, 2L))
  expect_identical(y$rm, c(1000L, 3000L, 4000L, NA, 8000L))
  expect_equal(y$distance_to_rm, c(873.50885850392, 535.63765010454, 754.230245890789, NA,
                                    514.952511361304))
})

test_that("fwa_add_nearest_id_to_rm", {
  x <- fwa_rm(blue_line_key = 356308001L, interval = 100, start = 10000)
  expect_is(x, "sf")

  x <- x[1:10,]
  y <- x[5:7,]

  chk::expect_chk_error(fwa_add_nearest_id_to_rm(x, y))
  y$new <- c(3L,5L,2L)
  expect_identical(fwa_add_nearest_id_to_rm(x, y[integer(0),], id = "new"),
                   dplyr::mutate(x, new = NA_integer_))
  expect_identical(fwa_add_nearest_id_to_rm(x[integer(0),], y, id = "new"),
                   dplyr::mutate(x, new = NA_integer_)[integer(0),])
  expect_identical(fwa_add_nearest_id_to_rm(x, dplyr::select(y, -blue_line_key) , id = "new"),
                   dplyr::mutate(x, new = c(NA, NA, NA, NA, 3L, 5L, 2L, NA, NA, NA)))
  expect_identical(fwa_add_nearest_id_to_rm(x, y , id = "new"),
                   dplyr::select(dplyr::mutate(x, new = c(NA, NA, NA, NA, 3L, 5L, 2L, NA, NA, NA)),
                                 blue_line_key, rm, new))
  expect_identical(fwa_add_nearest_id_to_rm(x, dplyr::select(y, -blue_line_key) , id = "new", max_distance = 100),
                   dplyr::mutate(x, new = c(NA, NA, NA, 3L, 3L, 5L, 2L, 2L, NA, NA)))

  y$blue_line_key[1] <- 1L
  expect_identical(fwa_add_nearest_id_to_rm(x, y , id = "new", max_distance = 100),
                   dplyr::select(dplyr::mutate(x, new = c(NA, NA, NA, NA, 5L, 5L, 2L, 2L, NA, NA)),
                                 blue_line_key, rm, new))
  x$blue_line_key[4] <- 1L
  expect_identical(fwa_add_nearest_id_to_rm(x, y , id = "new", max_distance = 100),
                   dplyr::select(dplyr::mutate(x, new = c(NA, NA, NA, 3L, 5L, 5L, 2L, 2L, NA, NA)),
                                 blue_line_key, rm, new))
  x$blue_line_key[5] <- NA
  expect_identical(fwa_add_nearest_id_to_rm(x, y , id = "new", max_distance = 100),
                   dplyr::select(dplyr::mutate(x, new = c(NA, NA, NA, 3L, 3L, 5L, 2L, 2L, NA, NA)),
                                 blue_line_key, rm, new))
  expect_identical(fwa_add_nearest_id_to_rm(x, y , id = "new", max_distance = 100, max_end_distance = 10),
                   dplyr::select(dplyr::mutate(x, new = c(NA, NA, NA, 3L, 3L, 5L, 2L, NA, NA, NA)),
                                 blue_line_key, rm, new))

  # not drop blue line keys that unrecognized!
  x$blue_line_key[6] <- 2L
  expect_identical(fwa_add_nearest_id_to_rm(x, y , id = "new", max_distance = 100, max_end_distance = 10),
                   dplyr::select(dplyr::mutate(x, new = c(NA, NA, NA, 3L, 3L, NA, 2L, NA, NA, NA)),
                                 blue_line_key, rm, new))

  # handles non-integer id
  y$new <- as.character(y$new)
  expect_identical(fwa_add_nearest_id_to_rm(x, y , id = "new", max_distance = 100, max_end_distance = 10),
                   dplyr::select(dplyr::mutate(x, new = c(NA, NA, NA, "3", "3", NA, "2", NA, NA, NA)),
                                 blue_line_key, rm, new))
})
