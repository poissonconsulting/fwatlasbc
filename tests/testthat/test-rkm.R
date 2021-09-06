test_that("fwa_rkm same as fwa_rm (after adjusting km)", {
  rlang::scoped_options(lifecycle_verbosity = "quiet")

  rkm <- fwa_rkm(blue_line_key = 356308001L, interval = 100, start = 10000)
  rkm <- dplyr::rename(rkm, rm = rkm)
  rkm$rm <- as.integer(rkm$rm * 1000)
  expect_identical(rkm, fwa_rm(blue_line_key = 356308001L, interval = 100, start = 10000))
})
