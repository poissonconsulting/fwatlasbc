test_that("fwa_mapview_rms works", {
  x <- fwa_add_rms_to_blk(data.frame(blk = c(356391970L, 356562108L)), interval = 5)
  expect_s4_class(fwa_mapview_rms(x, npoint = 10), "mapview")
  skip("need to test image snapshot")
})
