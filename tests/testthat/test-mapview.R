test_that("fwa_mapview works", {
  x <- fwa_add_rms_to_blk(data.frame(blk = c(356391970L, 356562108L)), interval = 1000)
  x$time <- hms::as_hms("12:01:02")
  expect_s4_class(fwa_mapview(x), "mapview")
  skip("need to test image snapshot")
})
