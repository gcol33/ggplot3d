# Contour lines on slices through a gridded scalar field

Cuts the field by the planes of
[`stat_slice_3d()`](https://gillescolling.com/ggplot3d/reference/stat_slice_3d.md)
and traces level lines of the interpolated field on each section by the
marching triangles of
[`geom_contour_3d()`](https://gillescolling.com/ggplot3d/reference/geom_contour_3d.md):
the field is linear on each section triangle, crossings on shared edges
are shared, and segments are chained into open lines and closed loops.
Computed variables: `level` (the default colour) and `slice`.

## Usage

``` r
stat_slice_contour_3d(
  mapping = NULL,
  data = NULL,
  geom = ggplot2::GeomPath,
  position = "identity",
  ...,
  xslice = NULL,
  yslice = NULL,
  zslice = NULL,
  slice = NULL,
  n = NULL,
  bins = NULL,
  binwidth = NULL,
  breaks = NULL,
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = TRUE
)

geom_slice_contour_3d(...)
```

## Arguments

- mapping, data, position, ..., na.rm, show.legend, inherit.aes:

  Standard layer arguments.

- geom:

  Geom prototype for keyed triangle corners.

- xslice, yslice, zslice:

  Positions of planes perpendicular to the x, y and z axes, in
  transformed position coordinates.

- slice:

  NULL, a plane made with
  [`sdf_plane()`](https://gillescolling.com/ggplot3d/reference/sdf_sphere.md),
  or a list of them, for slices in any direction.

- n:

  Lattice size per side for oblique planes; NULL uses the largest grid
  dimension. For stat_density_slice_3d() the density grid resolution,
  which also sets the lattice.

- bins, binwidth, breaks:

  Level controls in transformed z units. Explicit breaks take
  precedence.

## Examples

``` r
d <- expand.grid(x = seq(-1, 1, length.out = 12),
                 y = seq(-1, 1, length.out = 12), z = seq(-1, 1, length.out = 12))
d$value <- with(d, exp(-(x^2 + 2 * y^2 + z^2)))
ggplot2::ggplot(d, ggplot2::aes(x, y, z = z, value = value)) +
  geom_slice_3d(zslice = 0) + geom_slice_contour_3d(zslice = 0, bins = 6, colour = "white") +
  coord_3d()
```
