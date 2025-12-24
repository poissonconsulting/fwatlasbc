# Add Intersection to Geometry

Adds a logical column for each name in name in y indicating whether each
element of x intersects with the element of y.

## Usage

``` r
fwa_add_intersection_to_geometry(x, y)
```

## Arguments

- x:

  An sf data frame.

- y:

  An sf data frame with character column name.

## Value

A copy of x with a logical column for each name in name indicating
whether each element of x intersects with the element of y.
