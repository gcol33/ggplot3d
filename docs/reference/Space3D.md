# Extension prototype for three-dimensional spaces

Inherit with
[`ggplot2::ggproto()`](https://ggplot2.tidyverse.org/reference/ggproto.html)
to define charts, display models, canonical coordinates, distances,
geodesics and isometries. Points are finite numeric row matrices. See
[`coord_3d()`](https://gillescolling.com/ggplot3d/reference/coord_3d.md)
for the method contracts, homogeneous model output and subdivision
controls.

## Usage

``` r
Space3D
```

## Format

A ggproto prototype. Fields `charts`, `models` and `default_model` name
the supported coordinate representations. Required methods are
`to_canonical(data, chart)`, `from_canonical(X, model)`,
`distance(X, Y)`, `geodesic(X, Y, s)` and `act(g, X)`.

## Value

A prototype for space extensions accepted by
[`coord_3d()`](https://gillescolling.com/ggplot3d/reference/coord_3d.md).
