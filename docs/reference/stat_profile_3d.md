# Cross-sections of a surface along a line

Samples a surface given as x/y/z grid nodes along one or more section
lines and draws the vertical wall between the surface and `base`: a
cross-section, or with several lines a fence diagram. The surface is
interpolated linearly on the same triangles
[`geom_surface_3d()`](https://gillescolling.com/ggplot3d/reference/geom_surface_3d.md)
draws, so the top of the wall lies on the drawn surface. Samples are
spaced evenly along each line and include its vertices; samples outside
the surface are missing and break the wall.

## Usage

``` r
stat_profile_3d(
  mapping = NULL,
  data = NULL,
  geom = GeomBand3D,
  position = "identity",
  ...,
  line,
  n = 100L,
  base = -Inf,
  method = "grid",
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = TRUE
)
```

## Arguments

- mapping, data, position, ..., na.rm, show.legend, inherit.aes:

  Standard layer arguments.

- geom:

  Registered geom prototype for the sampled profile; the default draws
  [`geom_band_3d()`](https://gillescolling.com/ggplot3d/reference/geom_band_3d.md)
  walls.

- line:

  Section lines: an sf object or sfc of LINESTRING or MULTILINESTRING
  features, a data frame with x, y and an optional group column, or a
  two-column matrix.

- n:

  Number of evenly spaced samples per line, besides its vertices.

- base:

  Bottom of the wall in original z units; `-Inf` is the floor of the
  box.

- method:

  Surface triangulation, `"grid"` or `"delaunay"`, as in
  [`geom_surface_3d()`](https://gillescolling.com/ggplot3d/reference/geom_surface_3d.md).

## Value

A ggplot2 layer.

## Details

The computed variables are `distance` along the line in transformed x/y
units, `value` (the surface height in transformed z units) and `line`,
the index of the section line. With `geom = ggplot2::GeomPath` the stat
draws the profile line on the surface. Lines and data share one
coordinate system.

## Examples

``` r
d <- expand.grid(x = seq(0, 10, length.out = 21), y = seq(0, 10, length.out = 21))
d$z <- with(d, sin(x / 2) + cos(y / 3))
fence <- data.frame(x = c(1, 9, 1, 9), y = c(2, 2, 8, 8), group = c(1, 1, 2, 2))
ggplot2::ggplot(d, ggplot2::aes(x, y, z = z)) +
  geom_surface_3d(fill = "grey85", alpha = 0.5) +
  stat_profile_3d(line = fence, fill = "tan", colour = "grey30") +
  coord_3d()
ggplot2::ggplot(d, ggplot2::aes(x, y, z = z)) +
  geom_surface_3d(fill = "grey85") +
  stat_profile_3d(line = fence, geom = ggplot2::GeomPath, colour = "red", linewidth = 1) +
  coord_3d()
```
