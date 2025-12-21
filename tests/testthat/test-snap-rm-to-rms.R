test_that("fwa_snap_rm_to_rms works", {
  rlang::local_options(nocache = TRUE)

  rm <- fwa_add_rms_to_blk(data.frame(blk = 356308001))

  x <- rm[rm$rm %in% c(0, 2000, 5000, 6000, 7000), ]
  rm <- rm[rm$rm %in% c(0, 2000, 5000, 6000, 7000, 10000), ]
  x <- fwa_snap_rm_to_rms(x, rm)
  expect_s3_class(x, "sf")
  expect_identical(colnames(x), c("blk", "new_blk", "rm", "new_rm", "distance_to_new_rm", "elevation", "geometry"))
  expect_equal(x$blk, rep(356308001, 5))
  expect_identical(x$new_blk, x$blk)
  expect_equal(x$rm, c(0, 2000, 5000, 6000, 7000))
  expect_equal(x$new_rm, c(0, 2000, 5000, 6000, 7000))
  expect_equal(x$distance_to_new_rm, rep(0, 5))
  expect_s3_class(x$geometry, "sfc_POINT")
})

test_that("fwa_snap_rm_to_rms works new_blk", {
  rlang::local_options(nocache = TRUE)

  rm <- fwa_add_rms_to_blk(data.frame(blk = 356308001))

  x <- rm[rm$rm %in% c(0, 2000, 5000, 6000, 7000), ]
  rm <- rm[rm$rm %in% c(0, 2000, 5000, 6000, 7000, 10000), ]
  x$new_blk <- x$blk
  x <- fwa_snap_rm_to_rms(x, rm)
  expect_s3_class(x, "sf")
  expect_identical(colnames(x), c("blk", "new_blk", "rm", "new_rm", "distance_to_new_rm", "elevation", "geometry"))
  expect_equal(x$blk, rep(356308001, 5))
  expect_identical(x$new_blk, x$blk)
  expect_equal(x$rm, c(0, 2000, 5000, 6000, 7000))
  expect_equal(x$new_rm, c(0, 2000, 5000, 6000, 7000))
  expect_equal(x$distance_to_new_rm, rep(0, 5))
  expect_s3_class(x$geometry, "sfc_POINT")
})

test_that("fwa_snap_rm_to_rms works new_blk different", {
  rlang::local_options(nocache = TRUE)

  rm <- fwa_add_rms_to_blk(data.frame(blk = 356308001))

  x <- rm[rm$rm %in% c(0, 2000, 5000, 6000, 7000), ]
  rm <- rm[rm$rm %in% c(0, 2000, 5000, 6000, 7000, 10000), ]
  rm$blk <- rm$blk + 1L
  x$new_blk <- x$blk + 1L
  x <- fwa_snap_rm_to_rms(x, rm)
  expect_s3_class(x, "sf")
  expect_identical(colnames(x), c("blk", "new_blk", "rm", "new_rm", "distance_to_new_rm", "elevation", "geometry"))
  expect_equal(x$blk, rep(356308001, 5))
  expect_identical(x$new_blk, x$blk + 1L)
  expect_equal(x$rm, c(0, 2000, 5000, 6000, 7000))
  expect_equal(x$new_rm, c(0, 2000, 5000, 6000, 7000))
  expect_equal(x$distance_to_new_rm, rep(0, 5))
  expect_s3_class(x$geometry, "sfc_POINT")
})

test_that("fwa_snap_rm_to_rms works multiple blks", {
  rlang::local_options(nocache = TRUE)

  rm <- fwa_add_rms_to_blk(data.frame(blk = 356308001))

  x <- rm[rm$rm %in% c(0, 2000, 5000, 6000, 7000), ]
  rm <- rm[rm$rm %in% c(0, 2000, 5000, 6000, 7000, 10000), ]

  x2 <- x
  x2$blk <- 1
  rm2 <- rm
  rm2$blk <- 1

  x <- dplyr::bind_rows(x, x2)
  rm <- dplyr::bind_rows(rm, rm2)

  x <- fwa_snap_rm_to_rms(x, rm)
  expect_s3_class(x, "sf")
  expect_identical(colnames(x), c("blk", "new_blk", "rm", "new_rm", "distance_to_new_rm", "elevation", "geometry"))
  expect_equal(x$blk, c(rep(1, 5), rep(356308001, 5)))
  expect_identical(x$new_blk, x$blk)
  expect_equal(x$rm, c(0, 2000, 5000, 6000, 7000, 0, 2000, 5000, 6000, 7000))
  expect_equal(x$new_rm, c(0, 2000, 5000, 6000, 7000, 0, 2000, 5000, 6000, 7000))
  expect_equal(x$distance_to_new_rm, rep(0, 10))
  expect_s3_class(x$geometry, "sfc_POINT")
})

test_that("fwa_snap_rm_to_rms works multiple blks new_rms", {
  rlang::local_options(nocache = TRUE)

  rm <- fwa_add_rms_to_blk(data.frame(blk = 356308001))

  x <- rm[rm$rm %in% c(0, 2000, 5000, 6000, 7000), ]
  rm <- rm[rm$rm %in% c(0, 2000, 5000, 6000, 7000, 10000), ]

  x2 <- x
  x2$blk <- 1
  rm2 <- rm
  rm2$blk <- 1

  x <- dplyr::bind_rows(x, x2)
  rm <- dplyr::bind_rows(rm, rm2)

  x$new_rm <- c(NA, 2000, rep(NA, 3), 0, rep(NA, 4))

  x <- fwa_snap_rm_to_rms(x, rm)
  expect_s3_class(x, "sf")
  expect_identical(colnames(x), c("blk", "new_blk", "rm", "new_rm", "distance_to_new_rm", "elevation", "geometry"))
  expect_equal(x$blk, c(rep(1, 5), rep(356308001, 5)))
  expect_identical(x$new_blk, x$blk)
  expect_equal(x$rm, c(0, 2000, 5000, 6000, 7000, 0, 2000, 5000, 6000, 7000))
  expect_equal(x$new_rm, c(0, 2000, 5000, 6000, 7000, 0, 2000, 5000, 6000, 7000))
  expect_equal(x$distance_to_new_rm, rep(0, 10))
  expect_s3_class(x$geometry, "sfc_POINT")
})

test_that("fwa_snap_rm_to_rms works multiple blks to same new_blk", {
  rlang::local_options(nocache = TRUE)

  rm <- fwa_add_rms_to_blk(data.frame(blk = 356308001))

  x <- rm[rm$rm %in% c(0, 2000, 5000, 6000, 7000), ]
  rm <- rm[rm$rm %in% c(0, 2000, 5000, 6000, 7000, 10000), ]

  x2 <- x
  x2$blk <- 1
  rm2 <- rm
  rm2$blk <- 1

  rm$rm <- rm$rm + 1000

  x <- dplyr::bind_rows(x, x2)
  rm <- dplyr::bind_rows(rm, rm2)
  x$new_blk <- 356308001

  x <- fwa_snap_rm_to_rms(x, rm)
  expect_s3_class(x, "sf")
  expect_identical(colnames(x), c("blk", "new_blk", "rm", "new_rm", "distance_to_new_rm", "elevation", "geometry"))
  expect_equal(x$blk, c(rep(1, 5), rep(356308001, 5)))
  expect_equal(x$new_blk, rep(356308001, 10))
  expect_equal(
    x$rm,
    c(0L, 2000L, 5000L, 6000L, 7000L, 0L, 2000L, 5000L, 6000L, 7000L)
  )
  expect_equal(
    x$new_rm,
    c(1000, 3000, 6000, 7000, 8000, 1000, 3000, 6000, 7000, 8000)
  )
  expect_equal(x$distance_to_new_rm, rep(0, 10))
  expect_s3_class(x$geometry, "sfc_POINT")
})

test_that("fwa_snap_rm_to_rms doesn't snap mouth by default", {
  rm <- fwa_add_rms_to_blk(data.frame(blk = 356308001), nocache = FALSE)

  x <- rm[rm$rm %in% c(0, 3000, 6000), ]
  rm <- rm[rm$rm %in% c(3000, 6000, 9000), ]
  rm$rm <- c(3000, 6000, 0)

  x <- fwa_snap_rm_to_rms(x, rm)

  expect_s3_class(x, "sf")
  expect_identical(colnames(x), c("blk", "new_blk", "rm", "new_rm", "distance_to_new_rm", "elevation", "geometry"))
  expect_equal(x$blk, rep(356308001, 3))
  expect_identical(x$new_blk, x$blk)
  expect_equal(x$rm, c(0, 3000, 6000))
  expect_equal(x$new_rm, c(3000, 3000, 6000))
  expect_equal(x$distance_to_new_rm, c(2331.96951067872, 0, 0))
  expect_s3_class(x$geometry, "sfc_POINT")
})

test_that("fwa_snap_rm_to_rms does snap mouth using new_rm", {
  rm <- fwa_add_rms_to_blk(data.frame(blk = 356308001), nocache = FALSE)

  x <- rm[rm$rm %in% c(0, 3000, 6000), ]
  rm <- rm[rm$rm %in% c(3000, 6000, 9000), ]
  rm$rm <- c(3000, 6000, 0)

  x$new_rm <- c(0, NA, NA)

  x <- fwa_snap_rm_to_rms(x, rm)

  expect_s3_class(x, "sf")
  expect_identical(colnames(x), c("blk", "new_blk", "rm", "new_rm", "distance_to_new_rm", "elevation", "geometry"))
  expect_equal(x$blk, rep(356308001, 3))
  expect_identical(x$new_blk, x$blk)
  expect_equal(x$rm, c(0, 3000, 6000))
  expect_equal(x$new_rm, c(0, 3000, 6000))
  expect_equal(x$distance_to_new_rm, c(4222.41372431505, 0, 0))
  expect_s3_class(x$geometry, "sfc_POINT")
})

test_that("fwa_snap_rm_to_rms no x", {
  rm <- fwa_add_rms_to_blk(data.frame(blk = 356308001), nocache = FALSE)

  x <- rm[FALSE, ]
  rm <- rm[rm$rm %in% c(3000, 6000, 9000), ]
  rm$rm <- c(3000, 6000, 0)

  x <- fwa_snap_rm_to_rms(x, rm)

  expect_s3_class(x, "sf")
  expect_identical(colnames(x), c("blk", "new_blk", "rm", "new_rm", "distance_to_new_rm", "elevation", "geometry"))
  expect_equal(x$blk, integer(0))
  expect_identical(x$new_blk, x$blk)
  expect_equal(x$rm, numeric(0))
  expect_equal(x$new_rm, integer(0))
  expect_equal(x$distance_to_new_rm, numeric(0))
  expect_s3_class(x$geometry, "sfc_GEOMETRY")
})

test_that("fwa_snap_rm_to_rms no rm", {
  rm <- fwa_add_rms_to_blk(data.frame(blk = 356308001), nocache = FALSE)

  x <- rm[rm$rm %in% c(0, 3000, 6000), ]
  rm <- rm[FALSE, ]
  x <- fwa_snap_rm_to_rms(x, rm)

  expect_s3_class(x, "sf")
  expect_identical(colnames(x), c("blk", "new_blk", "rm", "new_rm", "distance_to_new_rm", "elevation", "geometry"))
  expect_equal(x$blk, rep(356308001, 3))
  expect_identical(x$new_blk, x$blk)
  expect_equal(x$rm, c(0, 3000, 6000))
  expect_equal(x$new_rm, rep(NA_integer_, 3))
  expect_equal(x$distance_to_new_rm, rep(NA_real_, 3))
  expect_s3_class(x$geometry, "sfc_POINT")
})

test_that("fwa_snap_rm_to_rms no x or rm", {
  rm <- fwa_add_rms_to_blk(data.frame(blk = 356308001), nocache = FALSE)

  x <- rm[FALSE, ]
  rm <- rm[FALSE, ]

  x <- fwa_snap_rm_to_rms(x, rm)

  expect_s3_class(x, "sf")
  expect_identical(colnames(x), c("blk", "new_blk", "rm", "new_rm", "distance_to_new_rm", "elevation", "geometry"))
  expect_equal(x$blk, integer(0))
  expect_identical(x$new_blk, x$blk)
  expect_equal(x$rm, numeric(0))
  expect_equal(x$new_rm, integer(0))
  expect_equal(x$distance_to_new_rm, numeric(0))
  expect_s3_class(x$geometry, "sfc_GEOMETRY")
})

test_that("fwa_snap_rm_to_rms new_rm errors if new_rm not in rm", {
  rm <- fwa_add_rms_to_blk(data.frame(blk = 356308001), nocache = FALSE)

  x <- rm[rm$rm %in% c(0, 3000, 6000), ]
  rm <- rm[rm$rm %in% c(3000, 6000, 9000), ]
  rm$rm <- c(3000, 6000, 0)
  x$new_rm <- c(3000, 6000, 9000)

  expect_error(
    fwa_snap_rm_to_rms(x, rm),
    "^All `x\\$new_rm` values must be in `rm\\$rm` by `blk`\\.$"
  )
})

test_that("fwa_snap_rm_to_rms new_rm errors if not sorted", {
  rm <- fwa_add_rms_to_blk(data.frame(blk = 356308001), nocache = FALSE)

  x <- rm[rm$rm %in% c(0, 3000, 6000), ]
  rm <- rm[rm$rm %in% c(3000, 6000, 9000), ]
  rm$rm <- c(3000, 6000, 0)
  x$new_rm <- c(3000, 6000, 0)

  expect_error(fwa_snap_rm_to_rms(x, rm), "^`x\\$new_rm` must be sorted\\.$")
})

test_that("fwa_snap_rm_to_rms not exceeds subsequent new_rm", {
  rlang::local_options(nocache = TRUE)

  rm <- fwa_add_rms_to_blk(data.frame(blk = 356308001))

  x <- rm[rm$rm %in% c(0, 2000, 5000, 6000, 7000), ]
  rm <- rm[rm$rm %in% c(1000, 3000, 4000, 8000, 7000, 5000), ]
  x$new_rm <- c(NA, 4000, NA, NA, NA)

  x <- fwa_snap_rm_to_rms(x, rm)
  expect_s3_class(x, "sf")
  expect_identical(colnames(x), c("blk", "new_blk", "rm", "new_rm", "distance_to_new_rm", "elevation", "geometry"))
  expect_equal(x$blk, rep(356308001, 5))
  expect_identical(x$new_blk, x$blk)
  expect_equal(x$rm, c(0, 2000, 5000, 6000, 7000))
  expect_equal(x$new_rm, c(1000, 4000, 5000, 7000, 7000))
  expect_equal(x$distance_to_new_rm, c(873.50885850392, 1256.91656917142, 0, 392.512382032925, 0))
  expect_s3_class(x$geometry, "sfc_POINT")
})

test_that("fwa_snap_rm_to_rms snap new_rm to mouth", {
  rlang::local_options(nocache = TRUE)

  rm <- fwa_add_rms_to_blk(data.frame(blk = 356308001))

  x <- rm[rm$rm %in% c(0, 2000, 5000, 6000, 7000), ]
  rm <- rm[rm$rm %in% c(1000, 3000, 4000, 8000, 7000, 5000), ]
  rm$rm <- c(1000, 3000, 4000, 8000, 7000, 0)
  x$new_rm <- c(NA, 4000, NA, NA, NA)

  x <- fwa_snap_rm_to_rms(x, rm, snap_mouths = TRUE)
  expect_s3_class(x, "sf")
  expect_identical(colnames(x), c("blk", "new_blk", "rm", "new_rm", "distance_to_new_rm", "elevation", "geometry"))
  expect_equal(x$blk, rep(356308001, 5))
  expect_identical(x$new_blk, x$blk)
  expect_equal(x$rm, c(0, 2000, 5000, 6000, 7000))
  expect_equal(x$new_rm, c(0, 4000, 7000, 7000, 8000))
  expect_equal(x$distance_to_new_rm, c(3971.42036810618, 1256.91656917142, 1161.55661705109, 392.512382032925, 1161.55661705109))
  expect_s3_class(x$geometry, "sfc_POINT")
})

test_that("fwa_snap_rm_to_rms snap new_rm to mouth already set", {
  rlang::local_options(nocache = TRUE)

  rm <- fwa_add_rms_to_blk(data.frame(blk = 356308001))

  x <- rm[rm$rm %in% c(0, 2000, 5000, 6000, 7000), ]
  rm <- rm[rm$rm %in% c(1000, 3000, 4000, 8000, 7000, 5000), ]
  rm$rm <- c(1000, 3000, 4000, 8000, 7000, 0)
  x$new_rm <- c(1000, 4000, NA, NA, NA)

  x <- fwa_snap_rm_to_rms(x, rm, snap_mouths = TRUE)
  expect_s3_class(x, "sf")
  expect_identical(colnames(x), c("blk", "new_blk", "rm", "new_rm", "distance_to_new_rm", "elevation", "geometry"))
  expect_equal(x$blk, rep(356308001, 5))
  expect_identical(x$new_blk, x$blk)
  expect_equal(x$rm, c(0, 2000, 5000, 6000, 7000))
  expect_equal(x$new_rm, c(1000, 4000, 7000, 7000, 8000))
  expect_equal(x$distance_to_new_rm, c(873.50885850392, 1256.91656917142, 1161.55661705109, 392.512382032925, 1161.55661705109))
  expect_s3_class(x$geometry, "sfc_POINT")
})

test_that("fwa_snap_rm_to_rms only allow increasing order", {
  rlang::local_options(nocache = TRUE)

  rm <- fwa_add_rms_to_blk(data.frame(blk = 356308001))

  x <- rm[rm$rm %in% c(0, 2000, 5000, 6000, 7000), ]
  rm <- rm[rm$rm %in% c(1000, 3000, 4000, 8000), ]
  rm$rm <- c(1000, 8000, 4000, 3000)

  x <- fwa_snap_rm_to_rms(x, rm)
  expect_s3_class(x, "sf")
  expect_identical(colnames(x), c("blk", "new_blk", "rm", "new_rm", "distance_to_new_rm", "elevation", "geometry"))
  expect_equal(x$blk, rep(356308001, 5))
  expect_identical(x$new_blk, x$blk)
  expect_equal(x$rm, c(0, 2000, 5000, 6000, 7000))
  expect_equal(x$new_rm, c(1000, 3000, 8000, 8000, 8000))
  expect_equal(x$distance_to_new_rm, c(
    873.50885850392, 2167.96002745135, 1455.35594246782, 1333.22900356052,
    1377.43784259615
  ))
  expect_s3_class(x$geometry, "sfc_POINT")
})

test_that("fwa_snap_rm_to_rms respects new_rm and only allow increasing order", {
  rlang::local_options(nocache = TRUE)

  rm <- fwa_add_rms_to_blk(data.frame(blk = 356308001))

  x <- rm[rm$rm %in% c(0, 2000, 5000, 6000, 7000, 8000), ]
  rm <- rm[rm$rm %in% c(0, 2000, 5000, 6000, 8000), ]
  rm$rm <- c(0, 2000, 7000, 6000, 8000)
  x$new_rm <- c(NA, NA, 6000, NA, NA, NA)

  x <- fwa_snap_rm_to_rms(x, rm)
  expect_s3_class(x, "sf")
  expect_identical(colnames(x), c("blk", "new_blk", "rm", "new_rm", "distance_to_new_rm", "elevation", "geometry"))
  expect_equal(x$blk, rep(356308001, 6))
  expect_identical(x$new_blk, x$blk)
  expect_equal(x$rm, c(0, 2000, 5000, 6000, 7000, 8000))
  expect_equal(x$new_rm, c(0, 2000, 6000, 7000, 7000, 8000))
  expect_equal(x$distance_to_new_rm, c(0, 0, 772.26037010328, 772.26037010328, 1161.55661705109, 0))
  expect_s3_class(x$geometry, "sfc_POINT")
})

test_that("fwa_snap_rm_to_rms interpolates block", {
  rlang::local_options(nocache = TRUE)

  rm <- fwa_add_rms_to_blk(data.frame(blk = 356308001))

  x <- rm[rm$rm %in% c(0, 1000, 2000, 3000, 4000, 8000), ]
  rm <- rm[rm$rm %in% c(0, 4000, 5000, 6000, 7000, 8000), ]
  rm$rm <- c(0, 4000, 1000, 2000, 3000, 8000)

  x <- fwa_snap_rm_to_rms(x, rm)
  expect_s3_class(x, "sf")
  expect_identical(colnames(x), c("blk", "new_blk", "rm", "new_rm", "distance_to_new_rm", "elevation", "geometry"))
  expect_equal(x$blk, rep(356308001, 6))
  expect_identical(x$new_blk, x$blk)
  expect_equal(x$rm, c(0, 1000, 2000, 3000, 4000, 8000))
  expect_equal(x$new_rm, c(0, 0, 1000, 2000, 3000, 8000))
  expect_equal(x$distance_to_new_rm, c(0, 873.50885850392, 1837.8850050135, 1333.22900356052, 812.482832175147, 0))
  expect_s3_class(x$geometry, "sfc_POINT")
})

test_that("fwa_snap_rm_to_rms interpolates blocks", {
  rlang::local_options(nocache = TRUE)

  rm <- fwa_add_rms_to_blk(data.frame(blk = 356308001))

  x <- rm[c(1:5, 1:5, 1:5), ]
  x$rm <- c(c(1, 4, 7, 10, 13), c(2, 5, 8, 11, 14), c(3, 6, 9, 12, 15))
  x <- x[order(x$rm), ]
  x$new_rm <- c(1, rep(NA, 14))

  rm <- x
  x <- fwa_snap_rm_to_rms(x, rm)
  expect_s3_class(x, "sf")
  expect_identical(colnames(x), c("blk", "new_blk", "rm", "new_rm", "distance_to_new_rm", "elevation", "geometry"))
  expect_equal(x$blk, rep(356308001, 15))
  expect_identical(x$new_blk, x$blk)
  expect_equal(x$rm, 1:15)
  expect_s3_class(x$geometry, "sfc_POINT")

  # TODO Joe to look into
  expect_equal(x$new_rm, c(1, 2, 3, 4, 5, 6, 7, 8, 10, 11, 12, 13, 13, 14, 15))

  # TODO Joe to look into
  expect_equal(x$distance_to_new_rm, c(
    0, 0, 0, 0, 0, 0, 0, 0, 535.637650106249, 0, 0, 775.632623248084,
    0, 0, 0
  ))
})

test_that("fwa_snap_rm_to_rms multiple blks to 1 blk", {
  rlang::local_options(nocache = TRUE)

  rm <- fwa_add_rms_to_blk(data.frame(blk = 356308001))

  x <- rm[rm$rm %in% c(0, 2000, 5000, 6000, 7000), ]
  rm <- rm[rm$rm %in% c(0, 2000, 5000, 6000, 7000, 10000), ]

  rm2 <- rm
  rm$blk <- 1
  rm$rm <- rm$rm + 1000

  rm <- dplyr::bind_rows(rm, rm2)
  x2 <- x
  x2$blk <- 2
  x <- dplyr::bind_rows(x, x2)
  x$new_blk <- 356308001

  x <- fwa_snap_rm_to_rms(x, rm)
  expect_s3_class(x, "sf")
  expect_identical(colnames(x), c("blk", "new_blk", "rm", "new_rm", "distance_to_new_rm", "elevation", "geometry"))
  expect_equal(x$blk, c(rep(2, 5), rep(356308001, 5)))
  expect_equal(x$new_blk, rep(356308001, 10))
  expect_equal(x$rm, c(0L, 2000L, 5000L, 6000L, 7000L, 0L, 2000L, 5000L, 6000L, 7000L))
  expect_equal(x$new_rm, c(0, 2000, 5000, 6000, 7000, 0, 2000, 5000, 6000, 7000))
  expect_equal(x$distance_to_new_rm, c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0))
  expect_s3_class(x$geometry, "sfc_POINT")
})
