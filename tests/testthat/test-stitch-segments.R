test_that("stiches together one stream that is straight", {
  test_stream <- st_sfc(
    sf::st_multilinestring(
      c(sf::st_linestring(matrix(c(0, 0, 1, 1), ncol = 2, byrow = TRUE)),
        sf::st_linestring(matrix(c(1.05, 1.05, 2, 2), ncol = 2, byrow = TRUE)),
        sf::st_linestring(matrix(c(2.1, 2.1, 3, 3), ncol = 2, byrow = TRUE))
      )
    ),
    crs = 26911
  )

  df <- data.frame(
    name = "stream 1",
    blk = 1,
    geometry = test_stream
  ) |>
    sf::st_set_geometry("geometry")

  output <- fwa_stitch_segments(df)

  expect_s3_class(output, "data.frame")
  expect_s3_class(output, "sf")
  expect_equal(colnames(output), c("name", "blk", "geometry"))
  expect_s3_class(output$geometry, "sfc_LINESTRING")
})

test_that("stiches together one stream that is zig zag", {
  test_stream <- st_sfc(
    st_multilinestring(
      c(
        st_linestring(matrix(c(0, 0, 1, 0.4), ncol = 2, byrow = TRUE)),
        st_linestring(matrix(c(1.05, 0.6, 1.05, 2), ncol = 2, byrow = TRUE)),
        st_linestring(matrix(c(1.05, 2.10, -1.0,  1), ncol = 2, byrow = TRUE)),
        st_linestring(matrix(c(-1.5, 1, -1, 3), ncol = 2, byrow = TRUE))
      )
    ),
    crs = 26911
  )

  df <- data.frame(
    name = "stream 1",
    blk = 1,
    geometry = test_stream
  ) |>
    sf::st_set_geometry("geometry")

  output <- fwa_stitch_segments(df)

  expect_s3_class(output, "data.frame")
  expect_s3_class(output, "sf")
  expect_equal(colnames(output), c("name", "blk", "geometry"))
  expect_s3_class(output$geometry, "sfc_LINESTRING")
})

test_that("stiches each individual stream", {
  test_stream_1 <- st_sfc(
    sf::st_multilinestring(
      c(sf::st_linestring(matrix(c(0, 0, 1, 1), ncol = 2, byrow = TRUE)),
        sf::st_linestring(matrix(c(1.05, 1.05, 2, 2), ncol = 2, byrow = TRUE)),
        sf::st_linestring(matrix(c(2.1, 2.1, 3, 3), ncol = 2, byrow = TRUE))
      )
    ),
    crs = 26911
  )

  test_stream_2 <- st_sfc(
    st_multilinestring(
      c(
        st_linestring(matrix(c(0, 0, 1, 0.4), ncol = 2, byrow = TRUE)),
        st_linestring(matrix(c(1.05, 0.6, 1.05, 2), ncol = 2, byrow = TRUE)),
        st_linestring(matrix(c(1.05, 2.10, -1.0,  1), ncol = 2, byrow = TRUE)),
        st_linestring(matrix(c(-1.5, 1, -1, 3), ncol = 2, byrow = TRUE))
      )
    ),
    crs = 26911
  )

  df <- data.frame(
    name = c("stream 1", "stream 2"),
    blk = c(1, 2),
    geometry = c(test_stream_1, test_stream_2)
  ) |>
    sf::st_set_geometry("geometry")

  output <- fwa_stitch_segments(df)

  expect_s3_class(output, "data.frame")
  expect_s3_class(output, "sf")
  expect_equal(colnames(output), c("name", "blk", "geometry"))
  expect_s3_class(output$geometry, "sfc_LINESTRING")
})

test_that("geometry column has a different name", {
  test_stream <- st_sfc(
    sf::st_multilinestring(
      c(sf::st_linestring(matrix(c(0, 0, 1, 1), ncol = 2, byrow = TRUE)),
        sf::st_linestring(matrix(c(1.05, 1.05, 2, 2), ncol = 2, byrow = TRUE)),
        sf::st_linestring(matrix(c(2.1, 2.1, 3, 3), ncol = 2, byrow = TRUE))
      )
    ),
    crs = 26911
  )

  df <- data.frame(
    name = "stream 1",
    blk = 1,
    geometry = test_stream
  ) |>
    sf::st_set_geometry("geometry") |>
    dplyr::rename(stuff = geometry)

  output <- fwa_stitch_segments(df)

  expect_s3_class(output, "data.frame")
  expect_s3_class(output, "sf")
  expect_equal(colnames(output), c("name", "blk", "stuff"))
  expect_s3_class(output$stuff, "sfc_LINESTRING")
})

test_that("errors if not active sf object", {
  test_stream <- st_sfc(
    sf::st_multilinestring(
      c(sf::st_linestring(matrix(c(0, 0, 1, 1), ncol = 2, byrow = TRUE)),
        sf::st_linestring(matrix(c(1.05, 1.05, 2, 2), ncol = 2, byrow = TRUE))
      )
    ),
    crs = 26911
  )

  df <- data.frame(
    name = "stream 1",
    blk = 1,
    geometry = test_stream
  )

  expect_chk_error(fwa_stitch_segments(df))
})

test_that("errors if not a dataframe", {
  test_stream <- st_sfc(
    sf::st_multilinestring(
      c(sf::st_linestring(matrix(c(0, 0, 1, 1), ncol = 2, byrow = TRUE)),
        sf::st_linestring(matrix(c(1.05, 1.05, 2, 2), ncol = 2, byrow = TRUE))
      )
    ),
    crs = 26911
  )

  expect_chk_error(fwa_stitch_segments(test_stream))
})

test_that("errors if tolerance is negative", {
  test_stream <- st_sfc(
    sf::st_multilinestring(
      c(sf::st_linestring(matrix(c(0, 0, 1, 1), ncol = 2, byrow = TRUE)),
        sf::st_linestring(matrix(c(1.05, 1.05, 2, 2), ncol = 2, byrow = TRUE))
      )
    ),
    crs = 26911
  )

  df <- data.frame(
    name = "stream 1",
    blk = 1,
    geometry = test_stream
  ) |>
    sf::st_set_geometry("geometry")

  expect_chk_error(fwa_stitch_segments(df, tolerance = -1))
})

test_that("errors if tolerance is 0", {
  test_stream <- st_sfc(
    sf::st_multilinestring(
      c(sf::st_linestring(matrix(c(0, 0, 1, 1), ncol = 2, byrow = TRUE)),
        sf::st_linestring(matrix(c(1.05, 1.05, 2, 2), ncol = 2, byrow = TRUE))
      )
    ),
    crs = 26911
  )

  df <- data.frame(
    name = "stream 1",
    blk = 1,
    geometry = test_stream
  ) |>
    sf::st_set_geometry("geometry")

  expect_chk_error(fwa_stitch_segments(df, tolerance = 0))
})

test_that("returns empty dataframe if empty", {
  test_stream <- st_sfc(
    sf::st_multilinestring(
      c(sf::st_linestring(matrix(c(0, 0, 1, 1), ncol = 2, byrow = TRUE)),
        sf::st_linestring(matrix(c(1.05, 1.05, 2, 2), ncol = 2, byrow = TRUE))
      )
    ),
    crs = 26911
  )

  df <- data.frame(
    name = "stream 1",
    blk = 1,
    geometry = test_stream
  ) |>
    sf::st_set_geometry("geometry")

  df <- df[0,]

  output <- fwa_stitch_segments(df)

  expect_identical(output, df)
})

test_that("early exit if only linestrings", {
  test_stream_1 <- st_sfc(
      sf::st_linestring(matrix(c(0, 0, 1, 1), ncol = 2, byrow = TRUE)),
    crs = 26911
  )

  test_stream_2 <- st_sfc(
    sf::st_linestring(matrix(c(1.05, 1.05, 2, 2), ncol = 2, byrow = TRUE)),
    crs = 26911
  )

  df <- data.frame(
    name = c("stream 1", "stream 2"),
    blk = c(1, 2),
    geometry = c(test_stream_1, test_stream_2)
  ) |>
    sf::st_set_geometry("geometry")

  output <- fwa_stitch_segments(df)

  expect_identical(output, df)
})

test_that("early exit if only linestrings", {
  test_stream_1 <- st_sfc(
    sf::st_multilinestring(
      c(sf::st_linestring(matrix(c(0, 0, 1, 1), ncol = 2, byrow = TRUE)),
        sf::st_linestring(matrix(c(1.05, 1.05, 2, 2), ncol = 2, byrow = TRUE)),
        sf::st_linestring(matrix(c(2.1, 2.1, 3, 3), ncol = 2, byrow = TRUE))
      )
    ),
    crs = 26911
  )

  test_stream_2 <- st_sfc(
    st_linestring(matrix(c(-1.5, 1, -1, 3), ncol = 2, byrow = TRUE)),
    crs = 26911
  )

  df <- data.frame(
    name = c("stream 1", "stream 2"),
    blk = c(1, 2),
    geometry = c(test_stream_1, test_stream_2)
  ) |>
    sf::st_set_geometry("geometry")

  output <- fwa_stitch_segments(df)

  expect_s3_class(output, "data.frame")
  expect_s3_class(output, "sf")
  expect_equal(colnames(output), c("name", "blk", "geometry"))
  expect_s3_class(output$geometry, "sfc_LINESTRING")
  expect_equal(nrow(output), 2)
})

test_that("tolerance doesn't let one segment be created so returns multilinestring", {
  test_stream_1 <- st_sfc(
    sf::st_multilinestring(
      c(sf::st_linestring(matrix(c(0, 0, 1, 1), ncol = 2, byrow = TRUE)),
        sf::st_linestring(matrix(c(1.05, 1.05, 2, 2), ncol = 2, byrow = TRUE)),
        sf::st_linestring(matrix(c(2.1, 2.1, 3, 3), ncol = 2, byrow = TRUE))
      )
    ),
    crs = 26911
  )

  df <- data.frame(
    name = c("stream 1"),
    blk = c(1),
    geometry = c(test_stream_1)
  ) |>
    sf::st_set_geometry("geometry")

  output <- fwa_stitch_segments(df, tolerance = 0.1)

  expect_s3_class(output, "data.frame")
  expect_s3_class(output, "sf")
  expect_equal(colnames(output), c("name", "blk", "geometry"))
  expect_s3_class(output$geometry, "sfc_MULTILINESTRING")
  expect_equal(nrow(output), 1)
})

test_that("tolerance doesn't let one segment be created so returns multilinestring even if linestring present", {
  test_stream_1 <- st_sfc(
    sf::st_multilinestring(
      c(sf::st_linestring(matrix(c(0, 0, 1, 1), ncol = 2, byrow = TRUE)),
        sf::st_linestring(matrix(c(1.05, 1.05, 2, 2), ncol = 2, byrow = TRUE)),
        sf::st_linestring(matrix(c(2.1, 2.1, 3, 3), ncol = 2, byrow = TRUE))
      )
    ),
    crs = 26911
  )

  test_stream_2 <- st_sfc(
    st_linestring(matrix(c(-1.5, 1, -1, 3), ncol = 2, byrow = TRUE)),
    crs = 26911
  )

  df <- data.frame(
    name = c("stream 1", "stream 2"),
    blk = c(1, 2),
    geometry = c(test_stream_1, test_stream_2)
  ) |>
    sf::st_set_geometry("geometry")

  output <- fwa_stitch_segments(df, tolerance = 0.1)

  expect_s3_class(output, "data.frame")
  expect_s3_class(output, "sf")
  expect_equal(colnames(output), c("name", "blk", "geometry"))
  expect_equal(as.character(sf::st_geometry_type(output)), c("MULTILINESTRING", "LINESTRING"))
  expect_equal(nrow(output), 2)
})

test_that("behaviour if nothing to stitch", {
  test_stream_1 <- st_sfc(
    sf::st_multilinestring(
      c(sf::st_linestring(matrix(c(0, 0, 1, 1), ncol = 2, byrow = TRUE)),
        sf::st_linestring(matrix(c(1, 1, 2, 2), ncol = 2, byrow = TRUE))
      )
    ),
    crs = 26911
  )

  df <- data.frame(
    name = c("stream 1"),
    blk = c(1),
    geometry = c(test_stream_1)
  ) |>
    sf::st_set_geometry("geometry")

  output <- fwa_stitch_segments(df)

  expect_s3_class(output, "data.frame")
  expect_s3_class(output, "sf")
  expect_equal(colnames(output), c("name", "blk", "geometry"))
  expect_s3_class(output$geometry, "sfc_LINESTRING")
  expect_equal(nrow(output), 1)
})
