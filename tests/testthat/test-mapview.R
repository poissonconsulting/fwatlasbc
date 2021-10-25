test_that("fwa_mapview_rms works", {
  x <- fwa_add_rms_to_blk(data.frame(blk = 356308001, interval = 100, start = 10001))
  expect_s4_class(fwa_mapview_rms(x, npoint = 4), "mapview")
})
