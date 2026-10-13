# fwa_join_stream_segments without source soft-deprecated if elevation

    Code
      x <- fwa_join_stream_segments(streams, elevation = TRUE)
    Condition
      Warning:
      Calling `fwa_join_stream_segments()` without specifying `source` was deprecated in fwatlasbc 0.0.2.
      i The default `source = "google"` is being used. Please set `source = "google"` or `source = "elevatr"`.

