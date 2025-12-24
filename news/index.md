# Changelog

## fwatlasbc 0.0.1.9019

- Skipped failing tests on R-universe.
- Added action to message slack channel with weekly R-CMD-check results.
- Added
  [`fwa_add_point_to_stream_measure()`](https://poissonconsulting.github.io/fwatlasbc/reference/fwa_add_point_to_stream_measure.md).
- Added
  [`fwa_get_segment_from_rms()`](https://poissonconsulting.github.io/fwatlasbc/reference/fwa_get_segment_from_rms.md)
  to get line geometry for cuts or sections.

## fwatlasbc 0.0.1.9018

- Added
  [`fwa_convert_stream_names_to_blks()`](https://poissonconsulting.github.io/fwatlasbc/reference/fwa_convert_stream_names_to_blks.md).

## fwatlasbc 0.0.1.9017

- Added `reverse` argument to allow streams to be reversed.
- [`fwa_convert_streams_to_rms()`](https://poissonconsulting.github.io/fwatlasbc/reference/fwa_convert_streams_to_rms.md)
  now buffers to deal with inaccuracies in streams network.
- Fixed
  [`fwa_convert_streams_to_rms()`](https://poissonconsulting.github.io/fwatlasbc/reference/fwa_convert_streams_to_rms.md)
  getting wrong half of split!

## fwatlasbc 0.0.1.9016

- Added `snap_mouths = FALSE` argument to
  [`fwa_snap_rm_to_rms()`](https://poissonconsulting.github.io/fwatlasbc/reference/fwa_snap_rm_to_rms.md).

## fwatlasbc 0.0.1.9015

- Added
  [`fwa_add_new_blk_rm_to_blk_rm()`](https://poissonconsulting.github.io/fwatlasbc/reference/fwa_add_new_blk_rm_to_blk_rm.md),
  [`fwa_join_stream_segments()`](https://poissonconsulting.github.io/fwatlasbc/reference/fwa_join_stream_segments.md),
  [`fwa_convert_streams_to_rms()`](https://poissonconsulting.github.io/fwatlasbc/reference/fwa_convert_streams_to_rms.md),
  [`fwa_mapview_rms_to_rms()`](https://poissonconsulting.github.io/fwatlasbc/reference/fwa_mapview_rms_to_rms.md),
  [`fwa_mapview_rms_to_rms()`](https://poissonconsulting.github.io/fwatlasbc/reference/fwa_mapview_rms_to_rms.md),
  and
  [`fwa_snap_rms_to_rms()`](https://poissonconsulting.github.io/fwatlasbc/reference/fwa_snap_rms_to_rms.md).

- [`fwa_snap_rm_to_point()`](https://poissonconsulting.github.io/fwatlasbc/reference/fwa_snap_rm_to_point.md)
  now retains blk if there is no match.

- Updated stream names.

- Added `nocache = getOption("fwa.nocache", FALSE)` argument.

- Replaced .`data$name` with `name` in select statements.

- [`fwa_add_watershed_to_blk()`](https://poissonconsulting.github.io/fwatlasbc/reference/fwa_add_watershed_to_blk.md)
  now provides general error if watershed is undefined for blk at rm.

- Collections no longer include `upstream_area_ha`.

- `distance_to_lon_lat` now rounded to mm.

## fwatlasbc 0.0.1.9014

- Added
  [`fwa_convert_rms_to_streams()`](https://poissonconsulting.github.io/fwatlasbc/reference/fwa_convert_rms_to_streams.md)
  to convert rms sf point object into sf linestring object.
- Added
  [`fwa_prune_rms()`](https://poissonconsulting.github.io/fwatlasbc/reference/fwa_prune_rms.md)
  to prune tips.
- Added `fwa_add_cut_rm()`.
- [`fwa_add_section_to_rms()`](https://poissonconsulting.github.io/fwatlasbc/reference/fwa_add_section_to_rms.md)
  now preserves geometry.
- [`fwa_add_section_to_rms()`](https://poissonconsulting.github.io/fwatlasbc/reference/fwa_add_section_to_rms.md)
  now preserves sf.

## fwatlasbc 0.0.1.9013

- Added
  [`fwa_add_downstream_split_to_rms()`](https://poissonconsulting.github.io/fwatlasbc/reference/fwa_add_downstream_split_to_rms.md).
- Soft-deprecated
  [`fwa_add_split_to_rms()`](https://poissonconsulting.github.io/fwatlasbc/reference/fwa_add_upstream_split_to_rms.md)
  for
  [`fwa_add_upstream_split_to_rms()`](https://poissonconsulting.github.io/fwatlasbc/reference/fwa_add_upstream_split_to_rms.md).

## fwatlasbc 0.0.1.9012

- Preserved point characteristics when adjusting with
  `fwa_swap_branches()`.

## fwatlasbc 0.0.1.9011

- Added
  [`fwa_mapview()`](https://poissonconsulting.github.io/fwatlasbc/reference/fwa_mapview.md).
- [`fwa_convert_stream_network_to_rms()`](https://poissonconsulting.github.io/fwatlasbc/reference/fwa_convert_stream_network_to_rms.md)
  now excludes channels with a stream order of 1 from being parents.
- Added `...` argument to
  [`fwa_snap_rm_to_point()`](https://poissonconsulting.github.io/fwatlasbc/reference/fwa_snap_rm_to_point.md)
  to allow assignment by additional columns.

## fwatlasbc 0.0.1.9010

- Switched to new `fwapgr` endpoint.

## fwatlasbc 0.0.1.9009

- Fixed internal `group_split()` so that active geometry is preserved.

## fwatlasbc 0.0.1.9008

- Added `swap_branches_rms()`,
  [`fwa_parent_stream_name_rms()`](https://poissonconsulting.github.io/fwatlasbc/reference/fwa_parent_stream_name_rms.md),
  [`fwa_parent_blk_rms()`](https://poissonconsulting.github.io/fwatlasbc/reference/fwa_parent_blk_rms.md),
  [`fwa_mapview_rms()`](https://poissonconsulting.github.io/fwatlasbc/reference/fwa_mapview_rms.md),
  and
  [`fwa_swap_branches_rms()`](https://poissonconsulting.github.io/fwatlasbc/reference/fwa_swap_branches_rms.md).
- Added `stream_name = fwatlasbc:fwa_stream_name` argument to
  [`fwa_add_blks_to_stream_name()`](https://poissonconsulting.github.io/fwatlasbc/reference/fwa_add_blks_to_stream_name.md).

## fwatlasbc 0.0.1.9007

- Added
  [`fwa_add_intersection_to_geometry()`](https://poissonconsulting.github.io/fwatlasbc/reference/fwa_add_intersection_to_geometry.md)
  and
  [`fwa_add_gm_elevation_to_point()`](https://poissonconsulting.github.io/fwatlasbc/reference/fwa_add_gm_elevation_to_point.md).
- Added `chunk_size` and `digits` arguments to
  [`fwa_add_gm_elevation_to_point()`](https://poissonconsulting.github.io/fwatlasbc/reference/fwa_add_gm_elevation_to_point.md).

## fwatlasbc 0.0.1.9006

- Renamed `fwa_add_rm_to_lon_lat()` to
  [`fwa_add_blk_to_lon_lat()`](https://poissonconsulting.github.io/fwatlasbc/reference/fwa_add_blk_to_lon_lat.md).

## fwatlasbc 0.0.1.9005

- Soft-deprecated
  [`fwa_add_collection_to_watershed()`](https://poissonconsulting.github.io/fwatlasbc/reference/fwa_add_collection_to_polygon.md)
  for
  [`fwa_add_collection_to_polygon()`](https://poissonconsulting.github.io/fwatlasbc/reference/fwa_add_collection_to_polygon.md).
- Changed behaviour of
  [`fwa_add_collection_to_watershed()`](https://poissonconsulting.github.io/fwatlasbc/reference/fwa_add_collection_to_polygon.md)
  so that it copies blue_line_key to blk if present.
- Added
  [`fwa_add_split_to_rms()`](https://poissonconsulting.github.io/fwatlasbc/reference/fwa_add_upstream_split_to_rms.md).

## fwatlasbc 0.0.1.9004

- Added
  [`fwa_convert_stream_network_to_rms()`](https://poissonconsulting.github.io/fwatlasbc/reference/fwa_convert_stream_network_to_rms.md).
- Renamed `fwa_add_section_to_rm()` to
  [`fwa_add_section_to_rms()`](https://poissonconsulting.github.io/fwatlasbc/reference/fwa_add_section_to_rms.md)

## fwatlasbc 0.0.1.9002

- Added `fwa_add_rm_to_lon_lat()` and `fwa_add_stream_name_to_blk()`.
- Exported `fwa_stream_name` data.
- Moved `rm` argument in
  [`fwa_add_watershed_to_blk()`](https://poissonconsulting.github.io/fwatlasbc/reference/fwa_add_watershed_to_blk.md)
  to column in argument x.

## fwatlasbc 0.0.1.9001

- Switched column names to snake case.
- In
  [`fwa_add_rms_to_blk()`](https://poissonconsulting.github.io/fwatlasbc/reference/fwa_add_rms_to_blk.md),
  removed z-coordinate from returned geometry column and added as a
  numeric elevation column.

## fwatlasbc 0.0.1

- Added a `NEWS.md` file to track changes to the package.
