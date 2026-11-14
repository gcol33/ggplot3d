# Parametric surfaces

Evaluates `fun(u, v)` on a regular (u, v) grid and triangulates it with
indexed vertices, each grid quad split along its shorter diagonal. A
closed direction joins its last row of quads back to the first samples
by index, so a torus or a sphere has no seam; the end of a closed range
is not sampled. A twisted closed direction reverses the other parameter
across the seam, which closes a Moebius strip or a Klein bottle. Samples
at coincident positions, such as the poles of a sphere, are welded into
one vertex.

## Usage

``` r
stat_parametric_3d(
  mapping = NULL,
  data = NULL,
  geom = GeomStatsSurface3D,
  position = "identity",
  ...,
  fun,
  u = c(0, 2 * pi),
  v = c(0, 2 * pi),
  n = 40L,
  closed = FALSE,
  twist = FALSE,
  args = list(),
  adaptive = FALSE,
  tolerance = 0.002,
  angle = 10,
  max_depth = 4L,
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = TRUE
)

geom_parametric_3d(...)
```

## Arguments

- mapping, data, position, ..., na.rm, show.legend, inherit.aes:

  Standard layer arguments.

- geom:

  Geom prototype for keyed triangle corners.

- fun:

  Vectorised function of u and v returning x, y and z as a list, data
  frame or three-column matrix.

- u, v:

  Parameter ranges.

- n:

  Number of samples along u and v, scalar or two integers.

- closed:

  Whether u and v wrap around, scalar or two logicals.

- twist:

  Whether crossing the seam of a closed direction reverses the other
  parameter, scalar or two logicals.

- args:

  Additional named arguments passed to fun.

- adaptive:

  Whether to refine the grid where it deviates from fun.

- tolerance:

  Midpoint deviation tolerance as a fraction of the bounding-box
  diagonal.

- angle:

  Normal angle tolerance in degrees.

- max_depth:

  Maximum number of refinement rounds.

## Details

With `adaptive = TRUE` each edge whose midpoint on the surface lies
further than `tolerance` times the bounding-box diagonal from the chord
midpoint, or whose endpoint normals differ by more than `angle` degrees,
is marked, and marked triangles are refined by conforming longest-edge
bisection, up to `max_depth` rounds. Midpoints are shared by the
triangles of an edge, so the refined surface has no cracks.

Vertex normals (nx, ny, nz) are the unit vector of r_u x r_v from
central differences of fun; where r_u x r_v vanishes (poles) the
area-weighted normal of the adjacent faces is used. Computed variables
`u`, `v`, `gaussian_curvature` and `mean_curvature` (from the first and
second fundamental forms, the mean curvature signed with respect to r_u
x r_v) are averaged over each face, as is `z_mean`. Derivative stencils
evaluate fun up to 1e-4 times the parameter range beyond the ends of an
open range. Coordinates returned by fun are positions in the transformed
scale space. Self-intersecting surfaces such as the Klein bottle are
split where they cross by the visibility engine.

## References

Rivara (1984). Algorithms for refining triangular grids suitable for
adaptive and multigrid techniques. International Journal for Numerical
Methods in Engineering 20, 745-756.
[doi:10.1002/nme.1620200412](https://doi.org/10.1002/nme.1620200412)

## Examples

``` r
torus <- function(u, v) list(x = (1 + 0.4 * cos(v)) * cos(u),
  y = (1 + 0.4 * cos(v)) * sin(u), z = 0.4 * sin(v))
ggplot2::ggplot() +
  geom_parametric_3d(fun = torus, n = c(36, 18), closed = TRUE) + coord_3d()
moebius <- function(u, v) list(x = (1 + v / 2 * cos(u / 2)) * cos(u),
  y = (1 + v / 2 * cos(u / 2)) * sin(u), z = v / 2 * sin(u / 2))
ggplot2::ggplot() +
  stat_parametric_3d(ggplot2::aes(fill = ggplot2::after_stat(u)), fun = moebius,
    v = c(-1, 1), n = c(48, 6), closed = c(u = TRUE, v = FALSE),
    twist = c(u = TRUE, v = FALSE)) + coord_3d()
```
