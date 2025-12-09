if (FALSE) {
  library(sf)

  # example 1, 3 segments straight line
  seg_1 <-
    st_linestring(
      matrix(
        c(
          0,   0,
          1,   1
        ),
        ncol = 2,
        byrow = TRUE
      )
    )

  seg_2 <- st_linestring(matrix(c(
    1.05, 1.05,
    2,    2
  ), ncol = 2, byrow = TRUE))

  seg_3 <- st_linestring(matrix(c(
    2.1,  2.1,
    3,    3
  ), ncol = 2, byrow = TRUE))


  test_stream_1 <- st_sfc(
    st_multilinestring(c(seg_1, seg_2, seg_3)),
    crs = 26911
  )

  mapview::mapview(test_stream_1)

  # example 2, 4 segments zig zag and not in order
  seg_1 <- st_linestring(matrix(c(
    0,   0,
    1,   0.4
  ), ncol = 2, byrow = TRUE))

  seg_2 <- st_linestring(matrix(c(
    1.05, 0.6,
    1.05,    2
  ), ncol = 2, byrow = TRUE))

  seg_3 <- st_linestring(matrix(c(
    1.05, 2.10,
    -1.0,  1
  ), ncol = 2, byrow = TRUE))

  seg_4 <- st_linestring(matrix(c(
    -1.5, 1,
    -1, 3
  ), ncol = 2, byrow = TRUE))

  test_stream_2 <- st_sfc(
    st_multilinestring(c(seg_1, seg_2, seg_3, seg_4)),
    crs = 26911
  )

  mapview::mapview(test_stream_2)


}


x <- create_segments_for_disconnects(test_stream_1)
mapview::mapview(x)

x <- create_segments_for_disconnects(test_stream_2, tolerance = 0.15)
mapview::mapview(x)



test_that("multiplication works", {
  test_stream_1

})
