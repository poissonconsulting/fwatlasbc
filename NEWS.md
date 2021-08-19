<!-- NEWS.md is maintained by https://cynkra.github.io/fledge, do not edit -->

# fwatlasbc 0.0.0.9003

- Added `epsg = getOption("fwa.epsg", 3005)` and `intersect = FALSE` arguments to `fwa_get_xx()` functions.
- Fix bug `fwa_nearest_rkm()` where not using blue_line_key when all same non-missing blue_line_key.
- Relax requirement that `fwa_add_nearest_id_to_rkm()` only positive integer id columns.
- Allow `fwa_add_end_id_to_rkm()` to handle not just integer id columns.
- Fix `fwa_add_end_id_to_rkm()` so not include additional columns!


# fwatlasbc 0.0.0.9002

- Soft-deprecated `fwa_add_columns_to_rkm()` for `fwa_add_end_id_to_rkm()`.
- Added `fwa_add_end_id_to_rkm()`.
- Add max_end_distance argument to fwa_add_nearest_id_to_rkm() to trim uppermost ends.
- Rename fwa_add_nearest_id() to fwa_add_nearest_id_to_rkm().


# fwatlasbc 0.0.0.9001

- Added fwa_add_nearest_id().
- fwa_rkm() rename distance_upstream to start and add end
- fwa_add_columns_to_rkm() returns tibble as opposed to data.frame.
- fwa_rkm() now returns blue_line_key as an integer.


# fwatlasbc 0.0.0.9000

* Added a `NEWS.md` file to track changes to the package.
