test_that("fwa_add_gm_elevation_to_point works few", {
  skip_on_runiverse()

  rm <- fwa_add_rms_to_blk(data.frame(blk = 356308001), interval = 1000)

  x <- fwa_add_gm_elevation_to_point(rm)
  expect_s3_class(x, "sf")
  expect_snapshot_data(x, "elevation_to_point")
})

test_that("fwa_add_gm_elevation_to_point works digits", {
  skip_on_runiverse()

  rm <- fwa_add_rms_to_blk(data.frame(blk = 356308001), interval = 1000)

  x <- fwa_add_gm_elevation_to_point(rm, digits = 3)
  expect_s3_class(x, "sf")
  expect_identical(colnames(x), c("blk", "rm", "elevation", "geometry"))
  expect_equal(
    x$elevation,
    c(
      5, 5, 5, 11.4579448699951, 5, 19.2013263702393, 10.6362476348877,
      6.682457447052, 6.72773838043213, 10.558274269104, 10.594612121582,
      8.23660659790039, 11.069766998291, 12.1544971466065, 11.0921401977539,
      11.8463001251221, 10.2729940414429, 80.8487396240234, 171.800216674805,
      210.517883300781
    )
  )
})

test_that("fwa_add_gm_elevation_to_point works almost 1000 (slightly different linux!)", {
  skip_on_runiverse()

  rm <- fwa_add_rms_to_blk(data.frame(blk = 356308001), interval = 20)

  x <- fwa_add_gm_elevation_to_point(rm)
  expect_s3_class(x, "sf")
  expect_identical(colnames(x), c("blk", "rm", "elevation", "geometry"))
  expect_identical(nrow(x), 962L)
  skip_on_os("linux")
  expect_snapshot_data(x, "elevation")
})

test_that("fwa_add_gm_elevation_to_point errors with chunk size of 320", {
  skip_on_runiverse()

  rm <- fwa_add_rms_to_blk(data.frame(blk = 356308001), interval = 20)

  expect_error(fwa_add_gm_elevation_to_point(rm, chunk_size = 320))
})
