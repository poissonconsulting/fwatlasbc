# fwa_add_watershed_to_blk empty polygon when no watershed returned

    Code
      x <- fwa_add_watershed_to_blk(data.frame(blk = c(1, 2, 3), rm = 0))
    Condition
      Warning:
      1 watershed is empty as no watershed was returned (the rm may be in the headwaters or beyond the end of the stream).

---

    Code
      x <- fwa_add_watershed_to_blk(data.frame(blk = c(1, 2, 4), rm = 0))
    Condition
      Warning:
      2 watersheds are empty as no watershed was returned (the rm may be in the headwaters or beyond the end of the stream).

