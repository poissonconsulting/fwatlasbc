test_that("fwa_nearest_rm works", {
  rm <- fwa_rm(blue_line_key = 356308001, interval = 1000)

  x <- rm[rm$rm %in% c(0, 2000, 5000, 6000, 7000),]
  rm <- rm[!rm$rm %in% c(0, 2000, 5000, 6000, 7000),]
  nearest <- fwa_nearest_rm(x, rm)
  expect_s3_class(nearest, "sf")
  expect_s3_class(nearest, "tbl_df")
  expect_identical(colnames(nearest), c("blue_line_key", "rm", "geometry", "distance_to_rm"))
  expect_identical(nrow(nearest), 5L)
  expect_s3_class(nearest$geometry, "sfc_POINT")
  expect_snapshot_data(nearest, "nearest")
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
  expect_equal(dplyr::as_tibble(y), dplyr::as_tibble(ey),
               ignore_attr = TRUE)

  x2 <- sf::st_zm(x2)
  is.na(x2$geometry[x2$rm == 6000]) <- TRUE
  y <- fwa_nearest_rm(x2, rm)
  ey <- sf::st_zm(x)
  is.na(ey$geometry[ey$rm == 6000]) <- TRUE
  ey$rm <- c(1000,3000,4000,NA,9000)
  ey$distance_to_rm <- c(873.50885850392, 535.63765010454, 754.230245890789, NA,
                         645.410842257104)
  y

  expect_equal(dplyr::as_tibble(y), dplyr::as_tibble(ey),
               ignore_attr = TRUE)

  is.na(x2$blue_line_key) <- TRUE
  y <- fwa_nearest_rm(x2, rm)

  expect_identical(y$blue_line_key, c(356308001L, 356308001L, 356308001L, NA, 2L))
  expect_identical(y$rm, c(1000L, 3000L, 4000L, NA, 8000L))
  expect_equal(y$distance_to_rm, c(873.50885850392, 535.63765010454, 754.230245890789, NA,
                                   514.952511361304))
})
