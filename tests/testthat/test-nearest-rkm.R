test_that("fwa_nearest_rkm same as fwa_nearest_rm", {
  rlang::scoped_options(lifecycle_verbosity = "quiet")

  rkm <- fwa_rkm(blue_line_key = 356308001, interval = 1000)
  rm <- fwa_rm(blue_line_key = 356308001, interval = 1000)

  x <- rkm[rkm$rkm %in% c(0, 2, 5, 6, 7),]
  rkm <- rkm[!rkm$rkm %in% c(0, 2, 5, 6, 7),]

  xrm <- rm[rm$rm %in% c(0, 2000, 5000, 6000, 7000),]
  rm <- rm[!rm$rm %in% c(0, 2000, 5000, 6000, 7000),]

  nearest <- fwa_nearest_rkm(x, rkm)
  nearest <- dplyr::rename(nearest, rm = rkm)
  nearest$rm <- as.integer(nearest$rm * 1000)
  expect_equal(nearest, fwa_nearest_rm(xrm, rm), ignore_attr = TRUE)
})
