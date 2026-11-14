# Trivariate density isosurfaces

Evaluates an axis-aligned Gaussian product kernel on transformed x/y/z
scale ranges. h contains Gaussian standard deviations in transformed
units. Automatic bandwidths use normal-reference marginal bandwidths
with sample size exponent -1/7. adjust multiplies automatic bandwidths.
Numeric levels are density thresholds; probs() specifies absolute
enclosed probability contents estimated by tensor trapezoidal
quadrature. Computed level and density contain the resolved density
threshold.

## Usage

``` r
stat_density_3d(
  mapping = NULL,
  data = NULL,
  geom = GeomStatsSurface3D,
  position = "identity",
  ...,
  h = NULL,
  adjust = 1,
  n = 32L,
  levels = NULL,
  caps = FALSE,
  space = "data",
  method = "cubes",
  simplify = NULL,
  clip = NULL,
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = TRUE
)

geom_isodensity_3d(...)
```

## Arguments

- mapping, data, position, ..., na.rm, show.legend, inherit.aes:

  Standard layer arguments.

- geom:

  Geom prototype for keyed triangle corners.

- h:

  Positive bandwidth, scalar or one per axis; NULL estimates it.

- adjust:

  Positive automatic bandwidth multiplier, scalar or one per axis.

- n:

  Grid resolution, scalar or three integers.

- levels:

  Numeric scalar-field thresholds; NULL uses pretty levels.

- caps:

  Whether to cap the sublevel region on the box faces (and on the
  clipping plane).

- space:

  `"data"` estimates in the position scales. `"ilr"` estimates a
  composition mapped with `a`, `b`, `c` (and `d`) in isometric log-ratio
  coordinates, where a Gaussian kernel respects relative scale, and maps
  the surface back into the simplex. With three parts, z stays as it is.

- method:

  Extraction method: `"cubes"`, `"tetra"`, `"dual"` or `"nets"`.

- simplify:

  NULL, or a target face count (or fraction of the extracted faces) to
  which the surface is reduced by quadric edge collapse
  ([`mesh_decimate()`](https://gillescolling.com/ggplot3d/reference/mesh_processing.md)).
  Normals are then recomputed from the decimated faces, or from `fun` in
  stat_isofunction_3d().

- clip:

  NULL, or a plane made with
  [`sdf_plane()`](https://gillescolling.com/ggplot3d/reference/sdf_sphere.md).
  The surface keeps the part on the side the plane normal points away
  from. With `caps = TRUE` the sublevel region is also closed on the
  plane, as with MATLAB's `isocaps`: each grid cell is split into the
  six tetrahedra of `method = "tetra"`, the plane cuts each in a
  triangle or quad on which the linear interpolant of the field is
  linear, and the part of that polygon below the level is filled, as on
  the box faces. Plane caps point along the plane normal. The plane is
  in transformed position coordinates.

## Examples

``` r
set.seed(1)
d <- data.frame(x = rnorm(80), y = rnorm(80), z = rnorm(80))
ggplot2::ggplot(d, ggplot2::aes(x, y, z = z)) +
  geom_isodensity_3d(h = 0.6, levels = 0.02, n = 12) + coord_3d()
ggplot2::ggplot(d, ggplot2::aes(x, y, z = z)) +
  stat_density_3d(h = 0.6, levels = 0.02, n = 8) + coord_3d()
```
