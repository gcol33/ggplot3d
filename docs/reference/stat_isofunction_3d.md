# Level surfaces of a trivariate function

Evaluates fun on transformed x/y/z scale ranges and passes the scalar
field to the same extraction used by stat_isosurface_3d. Since fun is
known everywhere, each vertex on a grid edge is then moved onto fun =
level by `refine` Illinois regula falsi steps inside the edge whose
endpoints bracket the level, and vertex normals (nx, ny, nz) are central
differences of fun at the final vertices, with steps of 1e-5 times each
axis range. Vertices that lie on no grid edge (merged `"tetra"`
clusters, centre vertices of `"cubes"`) keep their position and take the
normal from fun.

## Usage

``` r
stat_isofunction_3d(
  mapping = NULL,
  data = NULL,
  geom = GeomStatsSurface3D,
  position = "identity",
  ...,
  fun,
  n = 24L,
  args = list(),
  levels = NULL,
  caps = FALSE,
  method = NULL,
  refine = 2L,
  adaptive = FALSE,
  depth = c(4L, 8L),
  tol = 0.002,
  simplify = NULL,
  clip = NULL,
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = TRUE,
  render = c("mesh", "trace"),
  trace_tolerance = 1e-05,
  trace_steps = 512L
)

geom_isofunction_3d(...)
```

## Arguments

- mapping, data, position, ..., na.rm, show.legend, inherit.aes:

  Standard layer arguments.

- geom:

  Geom prototype for keyed triangle corners.

- fun:

  Vectorised scalar function of x, y and z, or a signed distance
  function
  ([`sdf_sphere()`](https://gillescolling.com/ggplot3d/reference/sdf_sphere.md)).

- n:

  Grid resolution, scalar or three integers.

- args:

  Additional named arguments passed to fun.

- levels:

  Numeric scalar-field thresholds; NULL uses pretty levels.

- caps:

  Whether to cap the sublevel region on the box faces (and on the
  clipping plane).

- method:

  Extraction method: `"cubes"` (the default on a grid), `"tetra"`,
  `"dual"` (the default with `adaptive = TRUE`) or `"nets"`.

- refine:

  Number of regula falsi steps per edge vertex; 0 keeps the linear
  interpolation of the gridded values.

- adaptive:

  Whether to extract on an octree refined by interval arithmetic instead
  of a grid.

- depth:

  Minimum and maximum octree depth; depth d splits each axis into 2^d
  intervals.

- tol:

  Geometric tolerance of the octree, relative to the diagonal of the
  scale box.

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

- render:

  `"mesh"` extracts triangles. `"trace"` sphere-traces an SDF with an
  opaque fill per raster sample. In the raster backend the traced
  surface composites at its depth with the other layers, translucent
  ones included; vector and hybrid panels place it as an image, and
  vector primitives behind it are hidden. Intrinsic primitives require
  backend_3d("raymarch"). Tracing uses transformed scale coordinates
  inside the panel box; these are geometry chart coordinates for the
  intrinsic backend.

- trace_tolerance:

  Surface tolerance in transformed scale units.

- trace_steps:

  Maximum sphere-tracing steps per ray.

## Details

`adaptive = TRUE` replaces the grid by an octree over the scale box
(Plantinga and Vegter 2004). fun is evaluated on whole boxes in interval
arithmetic, which encloses its range and gradient over each box. A box
whose range contains no level is dropped. Every other box is split down
to `depth[1]`, and further, up to `depth[2]`, while the inner product of
its gradient enclosure with itself may reach zero (the small normal
variation test: without it the level set inside the box can fold) or
while the box diagonal times r / \|c\| exceeds `tol` times the diagonal
of the scale box, where c and r are the centre and half diagonal of the
gradient enclosure. Every gradient in the box then lies within asin(r /
\|c\|) of c, so `tol` bounds how far the level set bends away from a
plane inside a box. Gradients are measured with each axis scaled to the
unit interval. The surface is then extracted by dual contouring on the
octree (Ju et al. 2002): each sign change on a minimal edge, an edge no
smaller box subdivides, gives one polygon through the vertices of the
three or four boxes around it, so boxes of different sizes join without
cracks. Each box carries one vertex per level-curve loop on its faces,
as with `method = "dual"` on a grid. Edge crossings are placed by
`refine` regula falsi steps and their normals come from fun. Only
`method = "dual"` (the default with `adaptive = TRUE`) and `"nets"`
apply; `n` is not used.

fun is evaluated on interval objects, so it may use `+`, `-`, `*`, `/`,
`^`, [`abs()`](https://rdrr.io/r/base/MathFun.html),
[`sqrt()`](https://rdrr.io/r/base/MathFun.html),
[`exp()`](https://rdrr.io/r/base/Log.html),
[`expm1()`](https://rdrr.io/r/base/Log.html),
[`log()`](https://rdrr.io/r/base/Log.html),
[`log1p()`](https://rdrr.io/r/base/Log.html),
[`log2()`](https://rdrr.io/r/base/Log.html),
[`log10()`](https://rdrr.io/r/base/Log.html),
[`sin()`](https://rdrr.io/r/base/Trig.html),
[`cos()`](https://rdrr.io/r/base/Trig.html),
[`sinh()`](https://rdrr.io/r/base/Hyperbolic.html),
[`cosh()`](https://rdrr.io/r/base/Hyperbolic.html),
[`tanh()`](https://rdrr.io/r/base/Hyperbolic.html),
[`atan()`](https://rdrr.io/r/base/Trig.html),
[`min()`](https://rdrr.io/r/base/Extremes.html),
[`max()`](https://rdrr.io/r/base/Extremes.html), and
[`pmin()`](https://rdrr.io/r/base/Extremes.html) and
[`pmax()`](https://rdrr.io/r/base/Extremes.html) called in its own body.
Any other operation, including comparisons and
[`ifelse()`](https://rdrr.io/r/base/ifelse.html), stops with an error
that names the call.

fun may be a signed distance function built with
[`sdf_sphere()`](https://gillescolling.com/ggplot3d/reference/sdf_sphere.md)
and
[`sdf_blend()`](https://gillescolling.com/ggplot3d/reference/sdf_blend.md).
levels then defaults to 0, and vertex normals (with the crossing normals
of `"dual"`) come from the exact gradient of its expression tree in
place of central differences.

## References

Dowell and Jarratt (1971). A modified regula falsi method for computing
the root of an equation. BIT 11, 168-174.
[doi:10.1007/BF01934364](https://doi.org/10.1007/BF01934364)

Plantinga and Vegter (2004). Isotopic approximation of implicit curves
and surfaces. Symposium on Geometry Processing, 251-260.
<https://diglib.eg.org/handle/10.2312/SGP.SGP04.251-260>

Ju, Losasso, Schaefer and Warren (2002). Dual contouring of Hermite
data. ACM Transactions on Graphics 21, 339-346.
[doi:10.1145/566654.566586](https://doi.org/10.1145/566654.566586)

## Examples

``` r
ggplot2::ggplot() +
  geom_isofunction_3d(fun = function(x, y, z) x^2 + y^2 + z^2,
                     n = 10, levels = 0.5) + coord_3d()
ggplot2::ggplot() +
  stat_isofunction_3d(fun = function(x, y, z) x^2 + y^2 + z^2,
                     n = 8, levels = 0.5) + coord_3d()
ggplot2::ggplot() +
  geom_isofunction_3d(fun = function(x, y, z) x^2 + y^2 + z^2,
                     levels = 0.5, adaptive = TRUE, depth = c(2, 4)) +
  coord_3d()
```
