test_that("fwa_snap_rm_to_point works", {
  rm <- fwa_add_rms_to_blk(data.frame(blk = 356308001))

  x <- rm[rm$rm %in% c(0, 2000, 5000, 6000, 7000),]
  rm <- rm[rm$rm %in% c(1000, 3000, 4000, 8000, 9000, 10000),]
  x <- fwa_snap_rm_to_point(x, rm)
  expect_s3_class(x, "sf")
  expect_identical(colnames(x), c("blk", "rm", "distance_to_rm", "elevation", "geometry"))
  expect_equal(x$blk, rep(356308001, 5))
  expect_equal(x$rm, c(1000, 3000, 4000, 4000, 8000))
  expect_equal(x$distance_to_rm, c(873.50885850392, 535.63765010454, 754.230245890789, 610.731097004499,
                             514.952511361304))
  expect_s3_class(x$geometry, "sfc_POINT")
})

test_that("fwa_snap_rm_to_point handles different projection", {
  rm <- fwa_add_rms_to_blk(data.frame(blk = 356308001))

  x <- rm[rm$rm %in% c(0, 2000, 5000, 6000, 7000),]
  rm <- rm[rm$rm %in% c(1000, 3000, 4000, 8000, 9000, 10000),]
  rm <- sf::st_transform(rm, 4326L)
  x <- fwa_snap_rm_to_point(x, rm)
  expect_s3_class(x, "sf")
  expect_identical(colnames(x), c("blk", "rm", "distance_to_rm", "elevation", "geometry"))
  expect_equal(x$blk, rep(356308001, 5))
  expect_equal(x$rm, c(1000, 3000, 4000, 4000, 8000))
  skip_on_os("linux")
  expect_equal(x$distance_to_rm, c(873.50885850392, 535.63765010454, 754.230245890789, 610.731097004499,
                                   514.952511361304))
  expect_s3_class(x$geometry, "sfc_POINT")
})

test_that("fwa_snap_rm_to_point handles different projection x", {
  rm <- fwa_add_rms_to_blk(data.frame(blk = 356308001))

  x <- rm[rm$rm %in% c(0, 2000, 5000, 6000, 7000),]
  rm <- rm[rm$rm %in% c(1000, 3000, 4000, 8000, 9000, 10000),]
  x <- sf::st_transform(x, 4326L)
  x <- fwa_snap_rm_to_point(x, rm)
  expect_s3_class(x, "sf")
  expect_identical(colnames(x), c("blk", "rm", "distance_to_rm", "elevation", "geometry"))
  expect_equal(x$blk, rep(356308001, 5))
  expect_equal(x$rm, c(1000, 3000, 4000, 4000, 8000))
  skip_on_os("linux")
  expect_equal(x$distance_to_rm, c(870.660879113271, 534.199200143721, 754.770979128795, 609.168343854328,
                                   513.588145823022))
  expect_s3_class(x$geometry, "sfc_POINT")
})

test_that("fwa_snap_rm_to_point handles missing blk", {
  rm <- fwa_add_rms_to_blk(data.frame(blk = 356308001))

  x <- rm[rm$rm %in% c(0, 2000, 5000, 6000, 7000),]
  rm <- rm[rm$rm %in% c(1000, 3000, 4000, 8000, 9000, 10000),]
  x$blk <- NULL
  x <- fwa_snap_rm_to_point(x, rm)
  expect_s3_class(x, "sf")
  expect_identical(colnames(x), c("rm", "distance_to_rm", "elevation", "geometry", "blk"))
  expect_equal(x$blk, rep(356308001, 5))
  expect_equal(x$rm, c(1000, 3000, 4000, 4000, 8000))
  expect_equal(x$distance_to_rm, c(873.50885850392, 535.63765010454, 754.230245890789, 610.731097004499,
                                   514.952511361304))
  expect_s3_class(x$geometry, "sfc_POINT")
})

test_that("fwa_snap_rm_to_point works one row", {
  rm <- fwa_add_rms_to_blk(data.frame(blk = 356308001))

  x <- rm[rm$rm == 5000,]
  rm <- rm[rm$rm %in% c(1000, 3000, 4000, 8000, 9000, 10000),]
  x <- fwa_snap_rm_to_point(x, rm)
  expect_s3_class(x, "sf")
  expect_identical(colnames(x), c("blk", "rm", "distance_to_rm", "elevation", "geometry"))
  expect_equal(x$blk, 356308001)
  expect_equal(x$rm, 4000)
  expect_equal(x$distance_to_rm, 754.230245890789)
  expect_s3_class(x$geometry, "sfc_POINT")
})

test_that("fwa_snap_rm_to_point works", {
  rm <- fwa_add_rms_to_blk(data.frame(blk = 356308001))

  x <- rm[rm$rm %in% c(0, 2000, 5000, 6000, 7000),]
  rm <- rm[rm$rm %in% c(1000, 3000, 4000, 9000, 10000),]
  rm$blk[rm$rm == 3000] <- 2L
  x$blk[1:2] <- NA_integer_
  x <- fwa_snap_rm_to_point(x, rm)
  expect_s3_class(x, "sf")
  expect_identical(colnames(x), c("blk", "rm", "distance_to_rm", "elevation", "geometry"))
  expect_equal(x$blk, c(356308001, 2, rep(356308001, 3)))
  expect_equal(x$rm, c(1000, 3000, 4000, 4000, 9000))
  expect_equal(x$distance_to_rm, c(873.50885850392, 535.63765010454, 754.230245890789, 610.731097004499,
                             645.410842257104))
  expect_s3_class(x$geometry, "sfc_POINT")
})

test_that("fwa_snap_rm_to_point works active geometry geometry", {
  rm <- fwa_add_rms_to_blk(data.frame(blk = 356308001))

  x <- rm[rm$rm %in% c(0, 2000, 5000, 6000, 7000),]
  rm <- rm[rm$rm %in% c(1000, 3000, 4000, 9000, 10000),]
  rm$blk[rm$rm == 3000] <- 2L
  x$blk[1:2] <- NA_integer_
  x$geometry2 <- rm$geometry[1:5]
  x <- fwa_snap_rm_to_point(x, rm)
  expect_s3_class(x, "sf")
  expect_identical(colnames(x), c("blk", "rm", "distance_to_rm", "elevation", "geometry", "geometry2"))
  expect_equal(x$blk, c(356308001, 2, rep(356308001, 3)))
  expect_equal(x$rm, c(1000, 3000, 4000, 4000, 9000))
  expect_equal(x$distance_to_rm, c(873.50885850392, 535.63765010454, 754.230245890789, 610.731097004499,
                                   645.410842257104))
  expect_s3_class(x$geometry, "sfc_POINT")
})

test_that("fwa_snap_rm_to_point works active geometry geometry2", {
  rm <- fwa_add_rms_to_blk(data.frame(blk = 356308001))

  x <- rm[rm$rm %in% c(0, 2000, 5000, 6000, 7000),]
  rm <- rm[rm$rm %in% c(1000, 3000, 4000, 9000, 10000),]
  rm$blk[rm$rm == 3000] <- 2L
  x$blk[1:2] <- NA_integer_
  x$geometry2 <- rm$geometry[1:5]
  x <- sf::st_set_geometry(x, "geometry2")
  expect_identical(sf::st_geometry(x), x$geometry2)
  x <- fwa_snap_rm_to_point(x, rm)
  expect_identical(sf::st_geometry(x), x$geometry2)

  expect_s3_class(x, "sf")
  expect_identical(colnames(x), c("blk", "rm", "distance_to_rm", "elevation", "geometry", "geometry2"))
  expect_equal(x$blk, c(356308001, 2, rep(356308001, 3)))
  expect_equal(x$rm, c(1000, 3000, 4000, 9000, 10000))
  expect_equal(x$distance_to_rm, c(0, 0, 0, 0, 0))
  expect_s3_class(x$geometry, "sfc_POINT")
})

test_that("fwa_snap_rm_to_point works segment", {
  rm <- fwa_add_rms_to_blk(data.frame(blk = 356308001))

  x <- rm[rm$rm %in% c(0, 2000, 5000, 6000, 7000),]
  rm <- rm[rm$rm %in% c(1000, 3000, 4000, 8000, 9000, 10000),]
  x$section <- c(2L, 1L, 2L, 2L, 3L)
  rm$section <- c(1L, 1L, 2L, 3L, 3L, 3L)

  x <- fwa_snap_rm_to_point(x, rm, section)
  expect_s3_class(x, "sf")
  expect_identical(colnames(x), c("blk", "rm", "distance_to_rm", "elevation", "geometry", "section"))
  expect_equal(x$blk, rep(356308001, 5))
  expect_equal(x$rm, c(4000, 3000, 4000, 4000, 8000))
  expect_equal(x$distance_to_rm, c(2954.89069472597, 535.63765010454, 754.230245890789, 610.731097004499,
                                   514.952511361304))
  expect_s3_class(x$geometry, "sfc_POINT")
})

test_that("fwa_snap_rm_to_point works segment no match", {
  rm <- fwa_add_rms_to_blk(data.frame(blk = 356308001))

  x <- rm[rm$rm %in% c(0, 2000, 5000, 6000, 7000),]
  rm <- rm[rm$rm %in% c(1000, 3000, 4000, 8000, 9000, 10000),]
  x$section <- c(2L, 1L, 2L, 2L, 4L)
  rm$section <- c(1L, 1L, 2L, 3L, 3L, 3L)

  x <- fwa_snap_rm_to_point(x, rm, section)
  expect_s3_class(x, "sf")
  expect_identical(colnames(x), c("blk", "rm", "distance_to_rm", "elevation", "geometry", "section"))
  expect_equal(x$blk, c(rep(356308001, 4), NA_integer_))
  expect_equal(x$rm, c(4000, 3000, 4000, 4000, NA_integer_))
  expect_equal(x$distance_to_rm, c(2954.89069472597, 535.63765010454, 754.230245890789, 610.731097004499,
                                   NA_real_))
  expect_s3_class(x$geometry, "sfc_POINT")
})

test_that("fwa_snap_rm_to_point works segment missing value x", {
  rm <- fwa_add_rms_to_blk(data.frame(blk = 356308001))

  x <- rm[rm$rm %in% c(0, 2000, 5000, 6000, 7000),]
  rm <- rm[rm$rm %in% c(1000, 3000, 4000, 8000, 9000, 10000),]
  x$section <- c(NA_integer_, 1L, 2L, 2L, 3L)
  rm$section <- c(1L, 1L, 2L, 3L, 3L, 3L)

  x <- fwa_snap_rm_to_point(x, rm, section)
  expect_s3_class(x, "sf")
  expect_identical(colnames(x), c("blk", "rm", "distance_to_rm", "elevation", "geometry", "section"))
  expect_equal(x$blk, c(rep(356308001, 5)))
  expect_equal(x$rm, c(1000, 3000, 4000, 4000, 8000))
  expect_equal(x$distance_to_rm, c(873.50885850392, 535.63765010454, 754.230245890789, 610.731097004499,
                                   514.952511361304))
  expect_s3_class(x$geometry, "sfc_POINT")
})

test_that("fwa_snap_rm_to_point works segment missing value rm", {
  rm <- fwa_add_rms_to_blk(data.frame(blk = 356308001))

  x <- rm[rm$rm %in% c(0, 2000, 5000, 6000, 7000),]
  rm <- rm[rm$rm %in% c(1000, 3000, 4000, 8000, 9000, 10000),]
  x$section <- c(1L, 1L, 2L, 2L, 3L)
  rm$section <- c(NA_integer_, 1L, 2L, 3L, 3L, 3L)

  x <- fwa_snap_rm_to_point(x, rm, section)
  expect_s3_class(x, "sf")
  expect_identical(colnames(x), c("blk", "rm", "distance_to_rm", "elevation", "geometry", "section"))
  expect_equal(x$blk, c(rep(356308001, 5)))
  expect_equal(x$rm, c(3000, 3000, 4000, 4000, 8000))
  expect_equal(x$distance_to_rm, c(2331.96951067872, 535.63765010454, 754.230245890789, 610.731097004499,
                                   514.952511361304))
  expect_s3_class(x$geometry, "sfc_POINT")
})

test_that("fwa_snap_rm_to_point works segment missing value x and rm", {
  rm <- fwa_add_rms_to_blk(data.frame(blk = 356308001))

  x <- rm[rm$rm %in% c(0, 2000, 5000, 6000, 7000),]
  rm <- rm[rm$rm %in% c(1000, 3000, 4000, 8000, 9000, 10000),]
  x$section <- c(NA_integer_, 1L, 2L, 2L, 3L)
  rm$section <- c(NA_integer_, 1L, 2L, 3L, 3L, 3L)

  x <- fwa_snap_rm_to_point(x, rm, section)
  expect_s3_class(x, "sf")
  expect_identical(colnames(x), c("blk", "rm", "distance_to_rm", "elevation", "geometry", "section"))
  expect_equal(x$blk, c(rep(356308001, 5)))
  expect_equal(x$rm, c(1000, 3000, 4000, 4000, 8000))
  expect_equal(x$distance_to_rm, c(873.50885850392, 535.63765010454, 754.230245890789, 610.731097004499,
                                   514.952511361304))
  expect_s3_class(x$geometry, "sfc_POINT")
})

test_that("fwa_snap_rm_to_point works segment missing value x and rm", {
  rm <- fwa_add_rms_to_blk(data.frame(blk = 356308001))

  x <- rm[rm$rm %in% c(0, 2000, 5000, 6000, 7000),]
  rm <- rm[rm$rm %in% c(1000, 3000, 4000, 8000, 9000, 10000),]
  x$section <- c(1L, 1L, 3L, 2L, 2L)
  rm$section <- c(2L, 2L, 2L, 2L, 2L, 2L)
  x$blk[1:3] <- NA_integer_
  rm2 <- rm[1:2,]
  rm2$blk <- 2L
  rm2$section <- c(1L, 1L)
  rm <- dplyr::bind_rows(rm, rm2)

  x <- fwa_snap_rm_to_point(x, rm, section)
  expect_s3_class(x, "sf")
  expect_identical(colnames(x), c("blk", "rm", "distance_to_rm", "elevation", "geometry", "section"))
  expect_equal(x$blk, c(2, 2, NA, 356308001, 356308001))
  expect_equal(x$rm, c(1000L, 3000L, NA, 4000L, 8000L))
  expect_equal(x$distance_to_rm, c(873.50885850392, 535.63765010454, NA, 610.731097004499,
                                   514.952511361304))
  expect_s3_class(x$geometry, "sfc_POINT")
})
