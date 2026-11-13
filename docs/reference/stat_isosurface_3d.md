# Level surfaces of a gridded scalar field

Requires a complete rectilinear x/y/z grid with a numeric value
aesthetic. Missing field values mask their incident cells. Coordinates
are transformed before extraction. Each cell is split into six
tetrahedra sharing a fixed body diagonal, giving consistent face
diagonals across adjacent cells. Linear interpolation within tetrahedra
avoids ambiguous cube cases. Field values within 64 machine epsilons of
a threshold, relative to the maximum absolute field value or threshold,
are snapped to that threshold. This gives shared grid vertices at
numerically coincident intersections. Finite-difference field gradients
are interpolated and normalised at vertices (nx, ny, nz). Lighting uses
the normalised mean of each face's vertex normals. caps = TRUE closes
the sublevel region on the grid box with outward faces. Surface triangle
winding points towards increasing field value.

## Usage

``` r
stat_isosurface_3d(
  mapping = NULL,
  data = NULL,
  geom = GeomStatsSurface3D,
  position = "identity",
  ...,
  levels = NULL,
  caps = FALSE,
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = TRUE
)

geom_isosurface_3d(...)
```

## Arguments

- mapping, data, position, ..., na.rm, show.legend, inherit.aes:

  Standard layer arguments.

- geom:

  Geom prototype for keyed triangle corners.

- levels:

  Numeric scalar-field thresholds; NULL uses pretty levels.

- caps:

  Whether to cap the sublevel region on the box faces.

## Details

Default levels are interior pretty(range(value), n = 10) values. The
number of levels depends on the pretty spacing. level and z_mean are
computed face attributes; .face, .corner and .vertex preserve triangle
topology.

## References

Doi and Koide (1991), An Efficient Method of Triangulating Equi-Valued
Surfaces by Using Tetrahedral Cells. IEICE E74-D, 214-224.
<https://globals.ieice.org/en_transactions/information/10.1587/e74-d_1_214/_p>

## Examples

``` r
d <- expand.grid(x = seq(-1, 1, length.out = 10),
                 y = seq(-1, 1, length.out = 10), z = seq(-1, 1, length.out = 10))
d$value <- with(d, x^2 + y^2 + z^2)
ggplot2::ggplot(d, ggplot2::aes(x, y, z = z, value = value)) +
  geom_isosurface_3d(levels = 0.6) + coord_3d()
ggplot2::ggplot(d, ggplot2::aes(x, y, z = z, value = value)) +
  stat_isosurface_3d(levels = 0.6) + coord_3d()
```
