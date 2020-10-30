test_that("rkm functions work", {

    blk <- 356308001
    drm <- 0
    drm2 <- 10000

    x <- fwa_rkm(blue_line_key = blk, interval = 100, start = drm2)
    expect_is(x, "sf")
    expect_identical(names(x), c("blue_line_key", "rkm", "geometry"))

  })
