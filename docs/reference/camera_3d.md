# Named camera views

Returns camera arguments for
[`coord_3d()`](https://gillescolling.com/ggplot3d/reference/coord_3d.md),
so a preset and further arguments combine with
[`do.call()`](https://rdrr.io/r/base/do.call.html). Explicit non-NULL
arguments override the preset.

## Usage

``` r
camera_3d(
  preset = c("default", "isometric", "dimetric", "trimetric", "top", "bottom", "front",
    "rear", "left", "right", "cabinet", "cavalier", "military"),
  projection = NULL,
  azimuth = NULL,
  elevation = NULL,
  roll = NULL,
  distance = NULL,
  fov = NULL,
  zoom = NULL,
  target = NULL,
  oblique = NULL,
  up = NULL
)
```

## Arguments

- preset:

  One of `"default"`, `"isometric"`, `"dimetric"` (x drawn at half the
  length of y and z), `"trimetric"`, the six principal views `"top"`,
  `"bottom"`, `"front"`, `"rear"`, `"left"` and `"right"` (each looking
  along one axis, as placed by
  [`facet_views()`](https://gillescolling.com/ggplot3d/reference/facet_views.md)),
  and the oblique views `"cabinet"` (depth at 45 degrees, half length),
  `"cavalier"` (depth at 45 degrees, full length) and `"military"` (the
  floor plan turned 45 degrees, heights drawn straight up at full
  length).

- projection, azimuth, elevation, roll, distance, fov, zoom, target,
  oblique, up:

  Overrides of the preset values; see
  [`coord_3d()`](https://gillescolling.com/ggplot3d/reference/coord_3d.md).

## Value

A named list of
[`coord_3d()`](https://gillescolling.com/ggplot3d/reference/coord_3d.md)
arguments.

## Examples

``` r
d <- data.frame(x = 1:3, y = c(1, 3, 2), z = c(2, 1, 3))
p <- ggplot2::ggplot(d, ggplot2::aes(x, y, z = z)) + geom_point_3d()
p + do.call(coord_3d, camera_3d("isometric"))
p + do.call(coord_3d, c(camera_3d("cabinet"), list(panes = "floor")))
```
