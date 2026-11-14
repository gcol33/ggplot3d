# Planar slices through a gridded scalar field

Cuts a complete rectilinear x/y/z grid with a numeric `value` aesthetic
by planes and colours each section by the field, as MATLAB's `slice`,
plotly's isosurface slices and PyVista's `slice()` do. Each plane is
sampled on a lattice of points and the field is interpolated trilinearly
within its grid cell. A plane perpendicular to an axis (`xslice`,
`yslice`, `zslice`, or a `slice` plane with an axis normal) is sampled
on the grid's own coordinates along the two other axes, so a slice on a
grid layer reproduces the grid values exactly; any other plane is
sampled on `n` x `n` points over the rectangle around its section with
the grid box, and the lattice triangles are clipped to that section.
Triangles with a missing corner value are dropped. With no slice given,
one slice per axis passes through the centre of the grid, as in
PyVista's `slice_orthogonal()`.

## Usage

``` r
stat_slice_3d(
  mapping = NULL,
  data = NULL,
  geom = GeomStatsSurface3D,
  position = "identity",
  ...,
  xslice = NULL,
  yslice = NULL,
  zslice = NULL,
  slice = NULL,
  n = NULL,
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = TRUE
)

geom_slice_3d(...)

stat_density_slice_3d(
  mapping = NULL,
  data = NULL,
  geom = GeomStatsSurface3D,
  position = "identity",
  ...,
  h = NULL,
  adjust = 1,
  n = 32L,
  xslice = NULL,
  yslice = NULL,
  zslice = NULL,
  slice = NULL,
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = TRUE
)

geom_density_slice_3d(...)
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

- h:

  Positive bandwidth, scalar or one per axis; NULL estimates it.

- adjust:

  Positive automatic bandwidth multiplier, scalar or one per axis.

## Details

Computed variables: `value`, the mean of the three corner values of each
triangle (fill is constant per face), and `slice`, a factor naming the
plane (`"x = 0.5"`, `"plane 1"`). The default fill is
`after_stat(value)`. Slices are not shaded; pass `shade = TRUE` to light
them.

stat_density_slice_3d() slices the Gaussian product-kernel density of
stat_density_3d(), computed on its `n` grid; `density` is the computed
density.

## References

MathWorks. slice: Volume slice planes.
<https://www.mathworks.com/help/matlab/ref/slice.html>

PyVista. DataObjectFilters.slice_orthogonal.
<https://docs.pyvista.org/api/core/_autosummary/pyvista.dataobjectfilters.slice_orthogonal>

## Examples

``` r
d <- expand.grid(x = seq(-1, 1, length.out = 12),
                 y = seq(-1, 1, length.out = 12), z = seq(-1, 1, length.out = 12))
d$value <- with(d, exp(-(x^2 + 2 * y^2 + z^2)))
ggplot2::ggplot(d, ggplot2::aes(x, y, z = z, value = value)) +
  geom_slice_3d() + coord_3d()
ggplot2::ggplot(d, ggplot2::aes(x, y, z = z, value = value)) +
  stat_slice_3d(slice = sdf_plane(c(1, -1, 1)), n = 30) + coord_3d()
set.seed(1)
p <- data.frame(x = rnorm(200), y = rnorm(200), z = rnorm(200))
ggplot2::ggplot(p, ggplot2::aes(x, y, z = z)) +
  geom_density_slice_3d(zslice = 0, n = 24) + coord_3d()
```
